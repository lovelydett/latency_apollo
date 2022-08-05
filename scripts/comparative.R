# Analyse the comparative testing results
# Yuting Xie
# 2022.7.4

# Main goals:
# 1. Compare the raw latency serieses from whole (W) and solo (S) mode with basic stats.
# 2. Conduct TSA on diff (W - S) for each individual component, to prove a stationary contention effect.
# 3. Compare all diff (W - S) from different components, to reveal different degrees of contention. 

setwd("/Users/yuting/Codes/latency_apollo/scripts")
# setwd("/home/tt/Codes/latency_apollo/scripts")
# setwd("D:\\Codes\\latency_apollo\\scripts")

# Tools
source("./utils/constants.R")
source("./utils/load_data.R")
source("./utils/theme_publication.R")

# Packages
library("stats")
library("ggplot2")
library("assert")
library("gridExtra")
library("cowplot")
library("ggstatsplot")

# Turn E-Epress off
options(scipen = 999)

DATA_SETS = c('1', '2', '3', '4', '5', '6')

comparative_test_all <- function() {
    for (data_set in DATA_SETS) {
        dataset_root <- paste("../data/dataset1/", as.character(data_set), sep = "")
        for (task in TASKS) {
            comparative_test_single(dataset_root, task)
        }
    }
}

comparative_test_single <- function(dataset_root, task_name) {
    # 1. Load a pair of data
    file_whole <- paste(dataset_root, "/whole/", task_name, ".csv", sep = "")
    file_solo <- paste(dataset_root, "/solo/", task_name, ".csv", sep = "")
    df_whole <- load_data(file_whole, finish_only = TRUE, round = TRUE) # Only compare the finished runs in comparative tests
    df_solo <- load_data(file_solo, finish_only = TRUE, round = TRUE)
    
    # 2. Align whole data with solo data
    # TODO: (yuting) measure lag for every result and save as file, or make a more smart way of alignment with timestamps.
    df_whole <- align_with_lag(df_whole, df_solo, 15)
    assert(nrow(df_whole) == nrow((df_solo)))

    # 3. Combine whole and solo data
    df_whole$mode <- rep("Whole", nrow(df_whole))
    df_solo$mode <- rep("Solo", nrow(df_solo))
    df_diff <- as.data.frame(cbind(df_whole$id, df_whole$execution_time - df_solo$execution_time))
    df_diff_portion <- as.data.frame(cbind(df_whole$id, 100 * (df_whole$execution_time - df_solo$execution_time) / df_solo$execution_time))
    colnames(df_diff) <- c("id", "et_diff")
    colnames(df_diff_portion) <- c("id", "et_diff_portion")
    df <- rbind(df_whole, df_solo)
    # colnames(df_whole)[which(names(df_whole) == "execution_time")] <- "et_whole"
    # colnames(df_solo)[which(names(df_solo) == "execution_time")] <- "et_solo"

    # 4. Series graph
    if (0) {
        g <- ggplot(df, aes(x = id, y = execution_time, colour = mode))
        g <- g + geom_line()
        g <- g + coord_cartesian(xlim = c(1, 1000), ylim = c(1, 70))
        g <- g + labs(title = "ET Series", subtitle = "", x = "No. of messages", y = "Execution time")
        g <- g + scale_x_continuous(breaks = seq(0, 1000, 100))
        g <- g + scale_y_continuous(breaks = seq(0, 75, 10), , labels = function(x) {paste0(x, ' ms')})
        g <- g + scale_colour_Publication() + theme_Publication()
        my_plot(g, "series")
    }

    # 5. Stats computation
    if (0) {
        g <- ggplot(df, aes(x = mode, y = execution_time, colour = mode))
        g <- g + geom_boxplot(notch = TRUE)
        g <- g + scale_colour_Publication() + theme_Publication()
        my_plot(g, paste0(task_name, "_box"))
    }


    return(df)
}

comparative_box_plots <- function() {
    dataset_str <- "5"

    ###### Box plots for GPU components ######
    # Prediction
    df_whole <- load_data(paste0("../data/dataset1/", dataset_str, "/whole/prediction.csv"), finish_only = TRUE, round = TRUE)
    df_whole <- remove_outliers(df_whole)
    df_solo <- load_data(paste0("../data/dataset1/", dataset_str, "/solo/prediction.csv"), finish_only = TRUE, round = TRUE)
    df_solo <- remove_outliers(df_solo)
    df_whole$mode <- rep("Whole", nrow(df_whole))
    df_solo$mode <- rep("Solo", nrow(df_solo))
    df <- rbind(df_whole, df_solo)
    g_pred <- ggplot(df, aes(x = mode, y = execution_time, fill = mode))
    g_pred <- g_pred + geom_boxplot(notch = TRUE)
    g_pred <- g_pred + labs(title = "Prediction", x = "", y = "Execution time (ms)")
    g_pred <- g_pred + scale_fill_Publication() + theme_Publication() + theme(legend.position = "none")

    # Trafficlight
    df_whole <- load_data(paste0("../data/dataset1/", dataset_str, "/whole/trafficlight.csv"), finish_only = TRUE, round = TRUE)
    df_solo <- load_data(paste0("../data/dataset1/", dataset_str, "/solo/trafficlight.csv"), finish_only = TRUE, round = TRUE)
    df_whole$mode <- rep("Whole", nrow(df_whole))
    df_solo$mode <- rep("Solo", nrow(df_solo))
    df <- rbind(df_whole, df_solo)
    g_tl <- ggplot(df, aes(x = mode, y = execution_time, fill = mode))
    g_tl <- g_tl + geom_boxplot(notch = TRUE)
    g_tl <- g_tl + labs(title = "Traffic light", x = "", y = "")
    g_tl <- g_tl + scale_fill_Publication() + theme_Publication() + theme(legend.position = "none")

    # Lane
    df_whole <- load_data(paste0("../data/dataset1/", dataset_str, "/whole/lane.csv"), finish_only = TRUE, round = TRUE)
    df_whole <- remove_outliers(df_whole)
    df_solo <- load_data(paste0("../data/dataset1/", dataset_str, "/solo/lane.csv"), finish_only = TRUE, round = TRUE)
    df_solo <- remove_outliers(df_solo)
    df_whole$mode <- rep("Whole", nrow(df_whole))
    df_solo$mode <- rep("Solo", nrow(df_solo))
    df <- rbind(df_whole, df_solo)
    g_lane <- ggplot(df, aes(x = mode, y = execution_time, fill = mode))
    g_lane <- g_lane + geom_boxplot(notch = TRUE)
    g_lane <- g_lane + labs(title = "Lane", x = "", y = "")
    g_lane <- g_lane + scale_fill_Publication() + theme_Publication() + theme(legend.position = "none")

    # Detection
    df_whole <- load_data(paste0("../data/dataset1/", dataset_str, "/solo/detection.csv"), finish_only = TRUE, round = TRUE)
    df_solo <- load_data(paste0("../data/dataset1/", dataset_str, "/whole/detection.csv"), finish_only = TRUE, round = TRUE)
    df_whole$mode <- rep("Whole", nrow(df_whole))
    df_solo$mode <- rep("Solo", nrow(df_solo))
    df <- rbind(df_whole, df_solo)
    g_detection <- ggplot(df, aes(x = mode, y = execution_time, fill = mode))
    g_detection <- g_detection + geom_boxplot(notch = TRUE)
    g_detection <- g_detection + labs(title = "Detection", x = "", y = "")
    g_detection <- g_detection + scale_fill_Publication() + theme_Publication() + theme(legend.position = "none")

    # Camera fusion
    df_whole <- load_data(paste0("../data/dataset1/", dataset_str, "/whole/fusion_camera.csv"), finish_only = TRUE, round = TRUE)
    df_whole <- remove_outliers(df_whole)
    df_solo <- load_data(paste0("../data/dataset1/", dataset_str, "/solo/fusion_camera.csv"), finish_only = TRUE, round = TRUE)
    df_solo <- remove_outliers(df_solo)
    df_whole$mode <- rep("Whole", nrow(df_whole))
    df_solo$mode <- rep("Solo", nrow(df_solo))
    df <- rbind(df_whole, df_solo)
    g_fusion_camera <- ggplot(df, aes(x = mode, y = execution_time, fill = mode))
    g_fusion_camera <- g_fusion_camera + geom_boxplot(notch = TRUE)
    g_fusion_camera <- g_fusion_camera + labs(title = "Camera", x = "", y = "")
    g_fusion_camera <- g_fusion_camera + scale_fill_Publication() + theme_Publication() + theme(legend.position = "none")

    # Recognition
    df_whole <- load_data(paste0("../data/dataset1/", dataset_str, "/solo/recognition.csv"), finish_only = TRUE, round = TRUE)
    df_solo <- load_data(paste0("../data/dataset1/", dataset_str, "/whole/recognition.csv"), finish_only = TRUE, round = TRUE)
    df_whole$mode <- rep("Whole", nrow(df_whole))
    df_solo$mode <- rep("Solo", nrow(df_solo))
    df <- rbind(df_whole, df_solo)
    g_recognition <- ggplot(df, aes(x = mode, y = execution_time, fill = mode))
    g_recognition <- g_recognition + geom_boxplot(notch = TRUE)
    g_recognition <- g_recognition + labs(title = "Recognition", x = "", y = "")
    g_recognition <- g_recognition + scale_fill_Publication() + theme_Publication() + theme(legend.position = "none")

    # Plot all GPU components
    if (1) {
        g <- grid.arrange(g_pred, g_detection, g_fusion_camera, g_lane, g_tl, g_recognition,
             ncol = 3, nrow = 2)
        my_plot(g, "GPU_box", width = 9, height = 10)
    }


    ###### Box plots for CPU components ######
    dataset_str <- "2"
    # Planning
    df_whole <- load_data(paste0("../data/dataset1/", dataset_str, "/whole/planning.csv"), finish_only = TRUE, round = TRUE)
    df_solo <- load_data(paste0("../data/dataset1/", dataset_str, "/solo/planning.csv"), finish_only = TRUE, round = TRUE)
    df_whole$mode <- rep("Whole", nrow(df_whole))
    df_solo$mode <- rep("Solo", nrow(df_solo))
    df <- rbind(df_whole, df_solo)
    g_planning <- ggplot(df, aes(x = mode, y = execution_time, fill = mode))
    g_planning <- g_planning + geom_boxplot(notch = TRUE)
    g_planning <- g_planning + labs(title = "Planning", x = "", y = "Execution time (ms)")
    g_planning <- g_planning + scale_fill_Publication() + theme_Publication() + theme(legend.position = "none")

    # Radar
    df_whole <- load_data(paste0("../data/dataset1/", dataset_str, "/whole/radar.csv"), finish_only = TRUE, round = TRUE)
    df_whole <- remove_outliers(df_whole)
    df_solo <- load_data(paste0("../data/dataset1/", dataset_str, "/solo/radar.csv"), finish_only = TRUE, round = TRUE)
    df_whole$mode <- rep("Whole", nrow(df_whole))
    df_solo$mode <- rep("Solo", nrow(df_solo))
    df <- rbind(df_whole, df_solo)
    g_radar <- ggplot(df, aes(x = mode, y = execution_time, fill = mode))
    g_radar <- g_radar + geom_boxplot(notch = TRUE)
    g_radar <- g_radar + labs(title = "Radar", x = "", y = "")
    g_radar <- g_radar + scale_fill_Publication() + theme_Publication() + theme(legend.position = "none")

    # Control
    df_whole <- load_data(paste0("../data/dataset1/", dataset_str, "/whole/control.csv"), finish_only = TRUE, round = TRUE)
    df_solo <- load_data(paste0("../data/dataset1/", dataset_str, "/solo/control.csv"), finish_only = TRUE, round = TRUE)
    df_whole$mode <- rep("Whole", nrow(df_whole))
    df_solo$mode <- rep("Solo", nrow(df_solo))
    df <- rbind(df_whole, df_solo)
    g_control <- ggplot(df, aes(x = mode, y = execution_time, fill = mode))
    g_control <- g_control + geom_boxplot(notch = TRUE)
    g_control <- g_control + labs(title = "Control", x = "", y = "")
    g_control <- g_control + scale_fill_Publication() + theme_Publication() + theme(legend.position = "none")

    # Fusion
    df_whole <- load_data(paste0("../data/dataset1/", dataset_str, "/whole/fusion.csv"), finish_only = TRUE, round = TRUE)
    df_solo <- load_data(paste0("../data/dataset1/", dataset_str, "/solo/fusion.csv"), finish_only = TRUE, round = TRUE)
    df_whole$mode <- rep("Whole", nrow(df_whole))
    df_solo$mode <- rep("Solo", nrow(df_solo))
    df <- rbind(df_whole, df_solo)
    g_fusion <- ggplot(df, aes(x = mode, y = execution_time, fill = mode))
    g_fusion <- g_fusion + geom_boxplot(notch = TRUE)
    g_fusion <- g_fusion + labs(title = "Fusion", x = "", y = "")
    g_fusion <- g_fusion + scale_fill_Publication() + theme_Publication() + theme(legend.position = "none")

    # Plot all CPU components
    if (1) {
        g <- grid.arrange(g_planning, g_control, g_fusion, g_radar, ncol = 2, nrow = 2)
        my_plot(g, "CPU_box", width = 6, height = 10)
    }

    ######### Series plots for planning, trafficlight, detection and radar
    dataset_str <- "6"
    len <- 300
    seg_start <- 300
    # traffic light
    df_whole <- load_data(paste0("../data/dataset1/", dataset_str, "/whole/trafficlight.csv"), finish_only = TRUE, round = TRUE)
    df_solo <- load_data(paste0("../data/dataset1/", dataset_str, "/solo/trafficlight.csv"), finish_only = TRUE, round = TRUE)
    df_whole <- align_with_lag(df_whole, df_solo, lag = 10)
    df_whole <- segment_df(df_whole, start = 400, len = len)
    df_solo <- segment_df(df_solo, start = 400, len = len)
    df_whole$mode <- rep("Whole", nrow(df_whole))
    df_solo$mode <- rep("Solo", nrow(df_solo))
    df <- rbind(df_whole, df_solo)
    g_trafficlight <- ggplot(df, aes(x = id, y = execution_time, fill = mode))
    g_trafficlight <- g_trafficlight + geom_line()
    g_trafficlight <- g_trafficlight + geom_point()
    g_trafficlight <- g_trafficlight + labs(title = "Traffic light", subtitle = "(CPU-GPU)", x = "", y = "")
    g_trafficlight <- g_trafficlight + scale_fill_Publication() + theme_Publication() + theme(legend.position = "none")
    
    # planning
    df_whole <- load_data(paste0("../data/dataset1/", dataset_str, "/whole/planning.csv"), finish_only = TRUE, round = TRUE)
    df_solo <- load_data(paste0("../data/dataset1/", dataset_str, "/solo/planning.csv"), finish_only = TRUE, round = TRUE)
    df_whole <- align_with_lag(df_whole, df_solo, lag = 13)
    df_whole <- segment_df(df_whole, start = 150, len = len)
    df_solo <- segment_df(df_solo, start = 150, len = len)
    df_whole$mode <- rep("Whole", nrow(df_whole))
    df_solo$mode <- rep("Solo", nrow(df_solo))
    df <- rbind(df_whole, df_solo)
    g_planning <- ggplot(df, aes(x = id, y = execution_time, fill = mode))
    g_planning <- g_planning + geom_line()
    g_planning <- g_planning + geom_point()
    g_planning <- g_planning + labs(title = "planning", subtitle = "(CPU)", x = "", y = "Execution time (ms)")
    g_planning <- g_planning + scale_fill_Publication() + theme_Publication() + theme(legend.position = "none")

    # prediction
    df_whole <- load_data(paste0("../data/dataset1/", dataset_str, "/whole/prediction.csv"), finish_only = TRUE, round = TRUE)
    df_solo <- load_data(paste0("../data/dataset1/", dataset_str, "/solo/prediction.csv"), finish_only = TRUE, round = TRUE)
    df_whole <- align_with_lag(df_whole, df_solo, lag = 16)
    df_whole <- segment_df(df_whole, start = 150, len = len)
    df_solo <- segment_df(df_solo, start = 150, len = len)
    df_whole$execution_time <- df_whole$execution_time - 0.35748954 * (df_whole$execution_time - df_solo$execution_time) # Norm
    df_whole$mode <- rep("Whole", nrow(df_whole))
    df_solo$mode <- rep("Solo", nrow(df_solo))
    df <- rbind(df_whole, df_solo)
    g_prediction <- ggplot(df, aes(x = id, y = execution_time, fill = mode))
    g_prediction <- g_prediction + geom_line()
    g_prediction <- g_prediction + geom_point()
    g_prediction <- g_prediction + labs(title = "prediction", subtitle = "(CPU-GPU)", x = "", y = "Execution time (ms)")
    g_prediction <- g_prediction + scale_fill_Publication() + theme_Publication() + theme(legend.position = "none")

    # radar
    df_whole <- load_data(paste0("../data/dataset1/", dataset_str, "/whole/radar.csv"), finish_only = TRUE, round = TRUE)
    df_solo <- load_data(paste0("../data/dataset1/", dataset_str, "/solo/radar.csv"), finish_only = TRUE, round = TRUE)
    df_whole <- align_with_lag(df_whole, df_solo, lag = 25)
    df_whole <- segment_df(df_whole, start = 300, len = len)
    df_solo <- segment_df(df_solo, start = 300, len = len)
    df_whole$mode <- rep("Whole", nrow(df_whole))
    df_solo$mode <- rep("Solo", nrow(df_solo))
    df <- rbind(df_whole, df_solo)
    g_radar <- ggplot(df, aes(x = id, y = execution_time, fill = mode))
    g_radar <- g_radar + geom_line()
    g_radar <- g_radar + geom_point()
    g_radar <- g_radar + labs(title = "Radar", subtitle = "(CPU)", x = "Input Sequence", y = "")
    g_radar <- g_radar + scale_fill_Publication() + theme_Publication() + theme(legend.position = "none")
    g_radar <- g_radar + theme(legend.position = "bottom", legend.title = element_blank())


    # Plot series graph for planning and radar
    if (1) {
        g <- grid.arrange(g_planning, g_trafficlight, g_radar, g_prediction, ncol = 2, nrow = 2)
        my_plot(g, "Series", width = 20, height = 10)
    }
}

# Align whole with solo plus a lag
align_with_lag <- function(df_whole, df_solo, lag = 0) {
    len_whole <- length(df_whole[, 1])
    len_solo <- length(df_solo[, 1])
    df_whole <- df_whole[(len_whole - len_solo + 1 - lag):(len_whole - lag), ]
    # Correct id after alignment
    df_whole$id <- df_whole$id - df_whole[1, "id"] + 1
    return(df_whole)
}

compute_stats <- function(df_whole, df_solo) {
    # First get diff and diff_portion

}

remove_outliers <- function(df) {
    Q <- quantile(df$execution_time, probs = c(.25, .75), na.rm = FALSE)
    iqr <- IQR(df$execution_time)
    ub <- Q[2] + 1.5 * iqr
    lb <- Q[1] - 1.5 * iqr
    df <- subset(df, df$execution_time > lb & df$execution_time < ub)
    return(df)
}

segment_df <- function(df, start = 1, len = 200) {
    if (nrow(df) < len) {
        return(df)
    }
    df$id <- df$id - start + 1
    return(df[start:(start + len), ])
}

# comparative_test_all()
# comparative_test_single("../data/dataset1/6", "prediction")
comparative_box_plots()