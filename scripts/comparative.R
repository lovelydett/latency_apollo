# Analyse the comparative testing results
# Yuting Xie
# 2022.7.4

# Main goals:
# 1. Compare the raw latency serieses from whole (W) and solo (S) mode with basic stats.
# 2. Conduct TSA on diff (W - S) for each individual component, to prove a stationary contention effect.
# 3. Compare all diff (W - S) from different components, to reveal different degrees of contention. 

# setwd("/Users/yuting/Codes/latency_apollo/scripts")
setwd("/home/tt/Codes/latency_apollo/scripts")

# Tools
source("./utils/constants.R")
source("./utils/load_data.R")
source("./utils/theme_publication.R")

# Packages
library("stats")
library("ggplot2")

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

    # 3. Get coressponding time serieses
    et_whole <- df_whole$execution_time # et = exectution time
    et_solo <- df_solo$execution_time # et = exectution time
    ts_whole <- ts(et_whole, start = 1)
    ts_solo <- ts(et_solo, start = 1)
    ts_diff <- ts(et_whole - et_solo, start = 1)
    if (0) {
        plot(ts_whole, col = "red")
        lines(ts_solo, col = "blue")
        lines(ts_diff, col = "green")
    }

    if (1) {
        g <- ggplot(df, aes(x = id, y = execution_time, color = component))
        g <- g + geom_line()
        g <- g + geom_point()
        g <- g + coord_cartesian(xlim = c(1, 1000), ylim = c(1, 70))
        g <- g + labs(title = "Title", subtitle = "Sub title", x = "X label", y = "Y label")
        g <- g + scale_x_continuous(breaks = seq(0, 1000, 100))
        g <- g + scale_y_continuous(breaks = seq(0, 75, 10), , labels = function(x) {paste0(x, ' ms')})
        plot(g)
    }

    # 3. Mean and Var for both modes
}

# Align whole with solo plus a lag (+ for right, - for left)
align_with_lag <- function(df_whole, df_solo, lag = 0) {
    len_whole <- length(df_whole[, 1])
    len_solo <- length(df_solo[, 1])
    df_whole <- df_whole[(len_whole - len_solo + 1 - lag):(len_whole - lag), ]
    return(df_whole)
}

# comparative_test_all()
comparative_test_single("../data/dataset1/6", "prediction")
