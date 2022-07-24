// Build graph model G({ET, ES}, V) given the dependencies of a modular input-driven system
// Yuting Xie
// 2022.7.19

#include <iostream>
#include <vector>
#include <array>
#include <queue>
#include <unordered_map>
#include <cassert>

// Relationships
enum Relationship {
    None = 0,
    SUB,
    TRIGGER,
    PUB
};

// Edge type can be task or synchronization
enum EdgeType {
    NONE = 0,
    TASK,
    SYNC_HARD,
    SYNC_SOFT,
    TASK_FROM,
    SYNC_HARD_FROM,
    SYNC_SOFT_FROM
};

// Example system:
// std::vector<std::vector<int>> R = {
//     {1, 1, TRIGGER},
//     {1, 4, PUB},
//     {2, 2, TRIGGER},
//     {2, 5, PUB},
//     {2, 6, PUB},
//     {3, 3, TRIGGER},
//     {3, 7, PUB},
//     {4, 4, TRIGGER},
//     {4, 5, TRIGGER},
//     {4, 8, PUB},
//     {5, 8, SUB},
//     {5, 6, SUB},
//     {5, 7, TRIGGER},
//     {5, 10, PUB},
// };

// An example system:
std::vector<std::vector<std::array<int, 2>>> M = {
    {},                                     // M[0] intentionally left empty
    {{1, TRIGGER}},                         // M[1] is the 1st channel, {1, TRIGGER} means chennel1 is necessary to trigger task1
    {{2, TRIGGER}},                         // M[2]
    {{3, TRIGGER}},                         // M[3]
    {{1, PUB}, {4, TRIGGER}},               // M[4]
    {{2, PUB}, {4, TRIGGER}, {5, SUB}},     // M[5]
    {{2, PUB}, {5, SUB}},                   // M[6]
    {{3, PUB}, {5, SUB}},                   // M[7]
    {{4, PUB}, {5, TRIGGER}},               // M[8]
    {{5, PUB}},                             // M[9]
};

typedef struct EdgeWithType {
    int dest;
    enum EdgeType type;
    EdgeWithType() : dest(-1), type(NONE) {}
    EdgeWithType(int dest, enum EdgeType type) : dest(dest), type(type) {}
} Edge;

typedef struct Vertex {
    float ts_ms; // Latest estimated ts_ms for the very next happen
    std::vector<Edge> edges_out;
    std::vector<Edge> edges_in;
} Node;

// Input: channel set M and the number of tasks n
std::vector<std::unordered_map<int, enum EdgeType>> build_graph(const std::vector<std::vector<std::array<int, 2>>> &M, int n) {
    // Phase 1: build a base graph with only the task edges, and ready-states and finish-states for each task.
    std::vector<std::unordered_map<int, enum EdgeType>> adjList; // For RVO
    adjList.resize(2 * n + 1); // Index of tasks goes from 1 -> n
    for (int i = 1; i <= n; ++i) {
        adjList[i][i + n] = TASK; // Index shift right by n, in such, the ready-state for i is i, the finish-state is i + n
    }

    // Phase 2: add the sync edges
    for (int i = 1; i < M.size(); ++i) { // For each channel (channels index from 1)
        // Saparate related tasks into three sets according to the relation.
        std::vector<int> sub_vec, pub_vec, trigger_vec;
        for (auto &r : M[i]) {
            auto task = r[0];
            auto relation = r[1];
            if (relation == SUB) {
                sub_vec.emplace_back(task);
            } else if (relation == TRIGGER) {
                trigger_vec.emplace_back(task);
            } else { // relation == PUB
                pub_vec.emplace_back(task);
            }
        }

        // Add sync edges: note that two sync edges could have identical end-points!
        for (auto &up : pub_vec) {
            for (auto &down : sub_vec) {
                // Link the finish-state of up and the ready-state of down
                std::cout << "Adding soft sync edge (" << up + n << ", " << down << ")" << std::endl; 
                adjList[up + n][down] = SYNC_SOFT;
            }
        }
        for (auto &up : pub_vec) {
            for (auto &down : trigger_vec) {
                // Link the finish-state of up and the ready-state of down
                std::cout << "Adding hard sync edge (" << up + n << ", " << down << ")" << std::endl;
                adjList[up + n][down] = SYNC_HARD;
            }
        }
    }

    // Phase 3 (optional): combine redundent states
    return adjList;
}

// Key point: every finish task must reset its corresponding value in E!!
// Inputs: 
// 1. Graph model in adj-list with latest timestamps
// 2. Next generation time for all sensors
// 3. Current estimation execution time for each task
// 4. Id of sensor of interest this time
// 5. Ids of states for exits (suppose only one exit for now)
float EIL(std::vector<Node> &adjList, 
          const std::unordered_map<int, float> &sensor_nxt,  
          const std::vector<float> &E,
          const int SOI,
          const std::vector<int> &SOE) {
    int m = adjList.size() - 1; // The number of states (vertexes)
    int n = m / 2; // The number of tasks

    // Start from every input sensors EXCEPT SOI, ts-BFS in each direction until first state with -1 value
    for (auto &it : sensor_nxt) {
        if (it.first == SOI) {
            continue;
        }
        adjList[it.first].ts_ms = it.second; // Next values for sensor states are guaranteed to be updated!
        std::queue<int> q;
        q.push(it.first);
        while (!q.empty()) {
            int cur = q.front();
            q.pop();
            // Process all out edges                                                               
            for (auto &[dest, type] : adjList[cur].edges_out) {
                if (adjList[dest].ts_ms > 0 && adjList[cur].ts_ms > adjList[dest].ts_ms
                     || type == SYNC_SOFT) {
                    // Indicate we still have an on-the-way piece of information, 
                    // or the happening of next state is not affected by current state, abort this path!
                    continue;
                }
                if (type == TASK) {
                    // For task edge, simply update the ts_ms for it (task id is always state id / 2)
                    adjList[dest].ts_ms = adjList[cur].ts_ms + E[cur / 2];
                    q.push(dest);
                } else if (type == SYNC_HARD) {
                    if (adjList[cur].ts_ms < adjList[dest].ts_ms) {
                        // Current state is fast, not affecting subsequent states, can abort this path!
                        continue;
                    }
                    adjList[dest].ts_ms = adjList[cur].ts_ms;
                    q.push(dest);
                } else {
                    assert(false);
                }

            }
        }
    }

    // Start from SOI, do ts-BFS that WONT STOP at state with ts > 0
    std::queue<int> q;
    q.push(SOI);
    while (!q.empty()) {
        int cur = q.front();
        q.pop();
        for (auto &[dest, type] : adjList[cur].edges_out) {
            if (type == TASK) {
                adjList[dest].ts_ms = adjList[cur].ts_ms + E[cur / 2];
                q.push(dest);
            } else if (type == SYNC_HARD && adjList[dest].ts_ms < adjList[cur].ts_ms) {
                adjList[dest].ts_ms = adjList[cur].ts_ms;
                q.push(dest);
            } else if (type == SYNC_SOFT) {
                // TODO: deal with the case that next state is expected to happen, but not yet happend!
                // By add one more 1/freq ? I dont know.
                continue;
            }
        }
    }

    // The final result is the diff of ts at SOE(s) and ts at SOI 
    return E[SOE[0]] - E[SOI];
}

void test_EIL() {
    
}

int main() {
    auto adjList = build_graph(M, 5);
    std::cout << "Graph constructed:\n";
    for (int i = 1; i < adjList.size(); ++i) {
        std::cout << i << ": ";
        for (auto &edge : adjList[i]) {
            std::cout << "(" << edge.first << "," << edge.second << ") ";
        }
        std::cout << std::endl;
    }
}