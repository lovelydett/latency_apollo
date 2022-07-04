# Analyse the comparative testing results
# Yuting Xie
# 2022.7.4

# Load data from csv, change latency into ms
load_data <- function(filename, finish_only=FALSE, round=FALSE) {
    df <- read.csv(filename, header = TRUE,  sep = ',',  stringsAsFactors = FALSE)
    df$execution_time <- df$execution_time * NS_TO_MS
    if (finish_only) {
        df <- df[df$is_finish == 1, ]
    }
    if (round) {
        ma <- max(df[df$execution_time < LATENCY_UB_MS, "execution_time"])
        df[df$execution_time >= LATENCY_UB_MS, "execution_time"] <- ma
    }
    return(df)
}