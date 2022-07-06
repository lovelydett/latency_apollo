# Analyse the comparative testing results
# Yuting Xie
# 2022.7.4

# Main goals:
# 1. Compare the raw latency serieses from whole (W) and solo (S) mode with basic stats.
# 2. Conduct TSA on diff (W - S) for each individual component, to prove a stationary contention effect.
# 3. Compare all diff (W - S) from different components, to reveal different degrees of contention. 

# setwd("/Users/yuting/Codes/latency_apollo/scripts")
# setwd("/home/tt/Codes/latency_apollo/scripts")
setwd("D:\\Codes\\latency_apollo\\scripts")

# Tools
source("./utils/constants.R")
source("./utils/load_data.R")
source("./utils/theme_publication.R")

# Packages
library("stats")
library("ggplot2")
library("assert")
library("gridExtra")

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

    # Prediction
    df_whole <- load_data(paste0("../data/dataset1/", dataset_str, "/whole/prediction.csv"), finish_only = TRUE, round = TRUE)
    df_solo <- load_data(paste0("../data/dataset1/", dataset_str, "/solo/prediction.csv"), finish_only = TRUE, round = TRUE)
    df_whole$mode <- rep("Whole", nrow(df_whole))
    df_solo$mode <- rep("Solo", nrow(df_solo))
    df <- rbind(df_whole, df_solo)
    g_pred <- ggplot(df, aes(x = mode, y = execution_time, colour = mode))
    g_pred <- g_pred + geom_boxplot(notch = TRUE)
    g_pred <- g_pred + scale_colour_Publication() + theme_Publication()

    # Trafficlight
    df_whole <- load_data(paste0("../data/dataset1/", dataset_str, "/whole/trafficlight.csv"), finish_only = TRUE, round = TRUE)
    df_solo <- load_data(paste0("../data/dataset1/", dataset_str, "/solo/trafficlight.csv"), finish_only = TRUE, round = TRUE)
    df_whole$mode <- rep("Whole", nrow(df_whole))
    df_solo$mode <- rep("Solo", nrow(df_solo))
    df <- rbind(df_whole, df_solo)
    g_tl <- ggplot(df, aes(x = mode, y = execution_time, colour = mode))
    g_tl <- g_tl + geom_boxplot(notch = TRUE)
    g_tl <- g_tl + scale_colour_Publication() + theme_Publication()

    # Lane
    df_whole <- load_data(paste0("../data/dataset1/", dataset_str, "/whole/lane.csv"), finish_only = TRUE, round = TRUE)
    df_solo <- load_data(paste0("../data/dataset1/", dataset_str, "/solo/lane.csv"), finish_only = TRUE, round = TRUE)
    df_whole$mode <- rep("Whole", nrow(df_whole))
    df_solo$mode <- rep("Solo", nrow(df_solo))
    df <- rbind(df_whole, df_solo)
    g_lane <- ggplot(df, aes(x = mode, y = execution_time, colour = mode))
    g_lane <- g_lane + geom_boxplot(notch = TRUE)
    g_lane <- g_lane + scale_colour_Publication() + theme_Publication()

    # Detection
    df_whole <- load_data(paste0("../data/dataset1/", dataset_str, "/whole/detection.csv"), finish_only = TRUE, round = TRUE)
    df_solo <- load_data(paste0("../data/dataset1/", dataset_str, "/solo/detection.csv"), finish_only = TRUE, round = TRUE)
    df_whole$mode <- rep("Whole", nrow(df_whole))
    df_solo$mode <- rep("Solo", nrow(df_solo))
    df <- rbind(df_whole, df_solo)
    g_detection <- ggplot(df, aes(x = mode, y = execution_time, colour = mode))
    g_detection <- g_detection + geom_boxplot(notch = TRUE)
    g_detection <- g_detection + scale_colour_Publication() + theme_Publication()

    # Camera fusion
    df_whole <- load_data(paste0("../data/dataset1/", dataset_str, "/whole/fusion_camera.csv"), finish_only = TRUE, round = TRUE)
    df_solo <- load_data(paste0("../data/dataset1/", dataset_str, "/solo/fusion_camera.csv"), finish_only = TRUE, round = TRUE)
    df_whole$mode <- rep("Whole", nrow(df_whole))
    df_solo$mode <- rep("Solo", nrow(df_solo))
    df <- rbind(df_whole, df_solo)
    g_fusion_camera <- ggplot(df, aes(x = mode, y = execution_time, colour = mode))
    g_fusion_camera <- g_fusion_camera + geom_boxplot(notch = TRUE)
    g_fusion_camera <- g_fusion_camera + scale_colour_Publication() + theme_Publication()

    # Recognition
    df_whole <- load_data(paste0("../data/dataset1/", dataset_str, "/whole/recognition.csv"), finish_only = TRUE, round = TRUE)
    df_solo <- load_data(paste0("../data/dataset1/", dataset_str, "/solo/recognition.csv"), finish_only = TRUE, round = TRUE)
    df_whole$mode <- rep("Whole", nrow(df_whole))
    df_solo$mode <- rep("Solo", nrow(df_solo))
    df <- rbind(df_whole, df_solo)
    g_recognition <- ggplot(df, aes(x = mode, y = execution_time, colour = mode))
    g_recognition <- g_recognition + geom_boxplot(notch = TRUE)
    g_recognition <- g_recognition + scale_colour_Publication() + theme_Publication()

    # Plot all GPU components
    g <- grid.arrange(g_pred, g_detection, g_fusion_camera, g_lane, g_tl, g_recognition,
             ncol = 3, nrow = 2)
    my_plot(g, "GPU")
    
}

# Align whole with solo plus a lag (+ for right, - for left)
align_with_lag <- function(df_whole, df_solo, lag = 0) {
    len_whole <- length(df_whole[, 1])
    len_solo <- length(df_solo[, 1])
    df_whole <- df_whole[(len_whole - len_solo + 1 - lag):(len_whole - lag), ]
    # Correct id after alignment
    df_whole$id <- df_whole$id - df_whole[1, "id"] + 1
    return(df_whole)
}

# comparative_test_all()
# comparative_test_single("../data/dataset1/6", "prediction")
comparative_box_plots()