# Analyse the comparative testing results
# Yuting Xie
# 2022.7.4

setwd("/home/tt/Codes/latency_apollo/scripts")

# Tools
source("./utils/constants.R")
source("./utils/load_data.R")

# Packages
library("stats")

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
    
    # TODO: allign whole and solo based on the length of solo mode result!
    len_whole <- length(df_whole[, 1])
    len_solo <- length(df_solo[, 1])
    print(len_whole)
    print(len_solo)
    df_whole <- df_whole[1 + len_whole - len_solo:len_whole, ]

    # 2. Get coressponding time serieses
    et_whole <- df_whole$execution_time # et = exectution time
    et_solo <- df_solo$execution_time # et = exectution time
    ts_whole <- ts(et_whole, start = 1)
    ts_solo <- ts(et_solo, start = 1)
    if (1) {
        plot(ts_whole, col = "red")
        lines(ts_solo, col = "blue")
    }

    # 3. Mean and Var for both modes

}

# comparative_test_all()
comparative_test_single("../data/dataset1/6", "prediction")
