# Analyse the comparative testing results
# Yuting Xie
# 2022.7.4

library("stats")

source("./utils/constants.R")

# Load data from csv, change latency into ms
load_data <- function(filename, finish_only = FALSE, round = FALSE, is_smooth = TRUE) {
    df <- read.csv(filename, header = TRUE,  sep = ',',  stringsAsFactors = FALSE)
    df$execution_time <- df$execution_time * NS_TO_MS
    if (finish_only) {
        df <- df[df$is_finish == 1, ]
    }
    if (round) {
        ma <- max(df[df$execution_time < LATENCY_UB_MS, "execution_time"])
        df[df$execution_time >= LATENCY_UB_MS, "execution_time"] <- ma + rnorm(1, 0, 3)
    }
    # Add id field for data frame
    df <- cbind(id = c(1 : length(df[, 1])), df)

    if (is_smooth) {
        df$execution_time <- smooth(df$execution_time)
    }
    
    return(df)
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