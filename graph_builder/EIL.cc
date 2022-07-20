// Build graph model G({ET, ES}, V) given the dependencies of a modular input-driven system
// Yuting Xie
// 2022.7.19

#include <iostream>
#include <vector>
#include <array>
#include <queue>
#include <unordered_map>

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
    SYNC_SOFT
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

// Input: 
float EIL(const std::vector<std::unordered_map<int, enum EdgeType>> &adjList, 
          std::vector<float> &sensor_nxt, 
          const std::vector<float> &E,
          const int sensor_generated,
          const float t_now) {
    int m = adjList.size() - 1; // The number of states (vertexes)
    int n = m / 2; // The number of tasks
    
    // Set the sensor_of_interest entry state as 0 to reveal the  
    int SOI = 

    // Start from every w
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