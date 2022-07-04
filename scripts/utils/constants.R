# Constants
# Yuting Xie
# 2022.7.4

# Times
MS_TO_NS <- 1e6
MS_TO_S <- 1e-3
NS_TO_MS <- 1e-6
S_TO_MS <- 1e3

# Thresholds
LATENCY_UB_MS <- 200

# Tasks
TASKS <- c(
    "detection",
    "recognition",
    "radar",
    "fusion",
    "fusion_camera",
    "planning",
    "prediction",
    "control"
)