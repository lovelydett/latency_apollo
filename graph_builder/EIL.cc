// Build graph model G({ET, ES}, V) given the dependencies of a modular input-driven system
// Yuting Xie
// 2022.7.19

#include <iostream>
#include <vector>
#include <array>
#include <queue>
#include <stack>
#include <functional>
#include <algorithm>
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
    BLOCKING,
    NON_BLOCKING
};

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

// Another example system:
std::vector<std::vector<std::array<int, 2>>> M2 = {
    {},                                     // M2[0] intentionally left empty
    {{1, TRIGGER}},                         // M2[1] is the 1st channel, {1, TRIGGER} means chennel1 is necessary to trigger task1
    {{2, TRIGGER}},                         // M2[2]
    {{3, TRIGGER}},                         // M2[3]
    {{1, PUB}, {4, TRIGGER}, {5, SUB}},     // M2[4]
    {{2, PUB}, {4, TRIGGER}},               // M2[5]
    {{2, PUB}, {5, TRIGGER}},               // M2[6]
    {{3, PUB}, {6, TRIGGER}},               // M2[7]
    {{4, PUB}, {7, TRIGGER}},               // M2[8]
    {{5, PUB}, {7, SUB}},                   // M2[9]
    {{6, PUB}, {7, TRIGGER}},
    {{7, PUB}}
};

typedef struct EdgeWithType {
    int dest;
    enum EdgeType type;
    EdgeWithType() : dest(-1), type(NONE) {}
    EdgeWithType(int dest, enum EdgeType type) : dest(dest), type(type) {}
} Edge;

typedef struct Vertex {
    float ts_ms = -1.f; // Latest estimated ts_ms for the very next happen
    float max_period = 0.f; // Yuting@2022.7.25: keep track of the slowest period info source to tackle the "cant catch up" problem.
    std::vector<Edge> edges_out;
    std::vector<Edge> edges_in;
} Node;

void print_graph(const std::vector<Node> &adjList, bool print_edge = false) {
    for (int i = 1; i < adjList.size(); ++i) {
        std::cout << "Node " << i <<
                     ": " << "ts_ms = " << adjList[i].ts_ms <<
                     ", " << "max_period = " << adjList[i].max_period << std::endl;
        if (!print_edge) {
            continue;
        }
        for (auto &edge : adjList[i].edges_out) {
            std::cout << "(" << edge.dest << ", " << edge.type << "), ";
        }
        std::cout << std::endl;
    }
}

// Input: channel set M and the number of tasks n
std::vector<Node> build_graph(const std::vector<std::vector<std::array<int, 2>>> &M, int n) {
    // Phase 1: build a base graph with only the task edges, and ready-states and finish-states for each task.
    std::vector<Node> adjList; // For RVO
    adjList.resize(2 * n + 1); // Index of tasks goes from 1 -> n
    for (int i = 1; i <= n; ++i) {
        adjList[i].edges_out.emplace_back(Edge(i + n, TASK)); // Index shift right by n, in such, the ready-state for i is i, the finish-state is i + n
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

        // Add sync edges: note that two types of edges could have identical end-points!
        for (auto &up : pub_vec) {
            for (auto &down : sub_vec) {
                // Link the finish-state of up and the ready-state of down
                std::cout << "Adding non_blocking edge (" << up + n << ", " << down << ")" << std::endl; 
                adjList[up + n].edges_out.emplace_back(Edge(down, NON_BLOCKING));
            }
        }
        for (auto &up : pub_vec) {
            for (auto &down : trigger_vec) {
                // Link the finish-state of up and the ready-state of down
                std::cout << "Adding blocking edge (" << up + n << ", " << down << ")" << std::endl;
                adjList[up + n].edges_out.emplace_back(Edge(down, BLOCKING));
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
// 3. Periods for all sensors.
// 4. Current estimation execution time for each task
// 5. Id of sensor of interest this time
// 6. Ids of states for exits (suppose only one exit for now)
float EIL(std::vector<Node> &adjList, 
          const std::unordered_map<int, float> &sensor_nxt,
          const std::unordered_map<int, float> &sensor_periods,
          const std::vector<float> &E,
          const int SOI,
          const std::vector<int> &SOE) {
    int m = adjList.size() - 1; // The number of states (vertexes)
    int n = m / 2; // The number of tasks

    // Initialize sensor states
    for (auto &it : sensor_periods) {
        adjList[it.first].max_period = it.second;
    }
    for (auto &it : sensor_nxt) {
        adjList[it.first].ts_ms = it.second;
    }

    std::function<void(int)> DFS_1, DFS_2;

    // Phase 1: global ts-BFS to update "next happening timestamp" for each state, EXCEPT SOI
    DFS_1 = [&](int cur_id) {
        for (auto &[dest, type] : adjList[cur_id].edges_out) {
            if (adjList[dest].ts_ms > 0 && adjList[cur_id].ts_ms > adjList[dest].ts_ms
                    || type == NON_BLOCKING) {
                // Indicate we still have an on-the-way piece of information, 
                // or the happening of next state is not affected by current state, abort this path!
                continue;
            }
            if (type == TASK) {
                // For task edge, simply update the ts_ms for it (task id is always state id / 2)
                adjList[dest].ts_ms = adjList[cur_id].ts_ms + E[cur_id];
                adjList[dest].max_period = std::max(adjList[dest].max_period, adjList[cur_id].max_period);
                std::cout << "Updating " << dest << " with " << cur_id << " for TASK to " << adjList[dest].ts_ms << std::endl;
                DFS_1(dest);
            } else if (type == BLOCKING) { 
                if (adjList[cur_id].ts_ms < adjList[dest].ts_ms) {
                    // Current state is fast, not affecting subsequent states, can abort this path!
                    continue;
                }
                adjList[dest].max_period = std::max(adjList[dest].max_period, adjList[cur_id].max_period);                   
                adjList[dest].ts_ms = adjList[cur_id].ts_ms;
                std::cout << "Updating " << dest << " with " << cur_id << " for BLOCKING to " << adjList[dest].ts_ms << std::endl;
                DFS_1(dest);
            } else {
                assert(false);
            }
        }
    };

    for (auto &it : sensor_nxt) {
        if (it.first == SOI) {
            continue;
        }
        DFS_1(it.first);
    }

    std::cout << "*** EIL: Phase 1 finished ***\n";
    print_graph(adjList);

    // Phase 2: start from SOI, do ts-DFS
    float ans = 1e6;
    DFS_2 = [&](int cur_id){
        if (adjList[cur_id].edges_out.size() == 0) {
            float t = ans;
            ans = std::min(ans, adjList[cur_id].ts_ms);
            std::cout << "Update ans = " << t << " to ans = " << ans << std::endl;
            return;
        }
        if (adjList[cur_id].ts_ms > ans) {
            return; // Pruning: no need to search a definitely not optimal path.
        }
        for (auto &[dest, type] : adjList[cur_id].edges_out) {
            if (type == TASK) {
                adjList[dest].ts_ms = adjList[cur_id].ts_ms + E[cur_id];
                DFS_2(dest); 
            } else if (type == BLOCKING && adjList[dest].ts_ms < adjList[cur_id].ts_ms) {
                adjList[dest].ts_ms = adjList[cur_id].ts_ms;
                DFS_2(dest); 
            } else if (type == NON_BLOCKING && adjList[dest].ts_ms < adjList[cur_id].ts_ms) {
                int k = int((adjList[cur_id].ts_ms - adjList[dest].ts_ms) / adjList[dest].max_period) + 1;
                float relax_coef = k * adjList[dest].max_period;
                adjList[dest].ts_ms += relax_coef;
                std::cout << "Node " << cur_id << " cant catch up Node " << dest << ", relax " << k << " periods, update it to " << adjList[dest].ts_ms << std::endl;
                DFS_2(dest);
                adjList[dest].ts_ms -= relax_coef;
            }
        }
    };
    DFS_2(SOI);

    std::cout << "*** EIL: Phase 2 finished ***\n";
    print_graph(adjList);

    // The final result is the diff of ts at SOE(s) and ts at SOI 
    return ans - adjList[SOI].ts_ms;
}

void test_EIL() {
    int num_tasks = 7;
    auto adjList = build_graph(M2, num_tasks);

    // Mock testing data
    std::unordered_map<int, float> sensor_nxt = {
        {1, 3},
        {2, 17},
        {3, 10}
    };
    std::unordered_map<int, float> sensor_periods = {
        {1, 20}, // Sensor with state_id = 1
        {2, 15}, // Sensor with state_id = 2
        {3, 5},  // ...
    };
    std::vector<float> E = {
        0,
        25, // T1
        5,  // T2
        12, // ... 
        7,
        29,
        2
    };
    int SOI = 1;
    std::vector<int> SOE = {14};

    // Conduct the query
    float res = EIL(adjList, sensor_nxt, sensor_periods, E, SOI, SOE);
    std::cout << "EIL for sensor with id = " << SOI << " is " << res << std::endl;
}

int main() {
    test_EIL();
}