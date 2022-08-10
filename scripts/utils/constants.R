# Constants
# Yuting Xie
# 2022.7.4

# Times
MS_TO_NS <- 1e6
MS_TO_S <- 1e-3
NS_TO_MS <- 1e-6
S_TO_MS <- 1e3

# Thresholds
LATENCY_UB_MS <- 100

# Tasks
TASKS <- c(
    "detection",
    "recognition",
    "radar",
    "fusion",
    "fusion_camera",
    "planning",
    "prediction",
    "control",
    "lane",
    "localization",
    "trafficlight"
)

MY_COLORS <- c(
    "#2470a0",
    "#DB2B39",
    "#1f640a",
    "#8E0C24",
    "#475F77",
    "#FF9C00",
    "#7B3B8C",
    "#00303F",
    "#de4307",
    "#393E41",
    "#330000",
    "#ED5485"
)

MY_COLORS_DEEP <- c(
    "#2d095c",
    "#333300",
    "#66a4ac",
    "#2470a0",
    "#09194F",
    "#606c70",
    "#20457c",
    "#217C7E",
    "#0387B1",
    "#2F6665",
    "#432B1B",
    "#594e4c",
    "#6981a7"
)

# DQPs
DQPS <- list(
    list("planning", 4, 1, 0),
    list("fusion", 15, 1, 0),
    list("prediction", 4, 1, 0),
    list("trafficlight", 7, 1, 0)
)

# Flags
FLAG_USE_SMOOTH = TRUE