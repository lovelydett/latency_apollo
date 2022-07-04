# Analyse the timing results recorded from Baidu Apollo
# Yuting Xie
# 2022.6.29

# Main tasks:
# 0. Data preparation
# 1. Comparative
#   1.1 ANOVA (or other time series comparison methods) applied on whole/solo results.
#   1.2 Derive some satistics to quantify the degree of contention.
# 2. Driving scenarios
#   2.1 Test the correlation between important component-specified information and latency.
#   2.2 Test the correlation between up-stream/down-stream tasks.
# 3. Communication Overheads
#   3.1 Connect the messages with the timestamp and compute the E2E latency difference between components.
#   3.2 Create the scheduling graph with the timestamp results.
# 4. Time series modeling
#   4.1 Stationary test for each component series.
#   4.2 White noise test for each component series.
#   4.3 AR/MA model to fit
#   4.4 GP model to fit the whole graph

# setwd("d:\\Codes\\latency_apollo\\scripts")
setwd("/Users/yuting/Codes/latency_apollo/scripts")

# Avoid scientific notion
options(scipen = 100)

# Load packages
library("vistime", help, pos = 2, lib.loc = NULL)
library("stats", help, pos = 2, lib.loc = NULL)

# Times
MS_TO_NS <- 1e6

# Task 1.1 Compare two time series
data_whole <- read.csv('../data/dataset1/2/whole/prediction.csv', header = TRUE,  sep = ',',  stringsAsFactors = FALSE)
data_solo <- read.csv('../data/dataset1/2/solo/prediction.csv', header = TRUE,  sep = ',',  stringsAsFactors = FALSE)

# Basic stats                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
ma <- max(data_whole[(data_whole$is_finish == 1) & (data_whole$execution_time < 100000000), "execution_time"])
mi <- min(data_whole[(data_whole$is_finish == 1) & (data_whole$execution_time < 100000000), "execution_time"])

data_whole[data_whole$execution_time > 100000000 , "execution_time"] = ma

runtime_whole <- data_whole[data_whole$is_finish == 1 , "execution_time"] / MS_TO_NS
runtime_solo <- data_solo[data_solo$is_finish == 1, "execution_time"] / MS_TO_NS
print("Whole mode:")
print(sprintf("Mean: %.2fms", mean(runtime_whole)))
print(sprintf("SD: %.2fms", sd(runtime_whole)))
print("Solo mode:")
print(sprintf("Mean: %.2fms", mean(runtime_solo)))
print(sprintf("Mean: %.2fms", sd(runtime_solo)))

# Basic plots
boxplot(runtime_whole, runtime_solo,
    main = "Comparision between whole and solo modes",
    at = c(1,2),
    names = c("Whole", "Solo"),
    las = 2,
    col = c("blue", "red"),
    border = "brown",
    horizontal = FALSE,
    notch = TRUE
)

# Gannt diagrams
# picked_df <- data_whole[6004:6028, c("ts_start", "ts_end", "component", "execution_time")]
# origin <- picked_df$ts_start[1] %/% MS_TO_NS
# picked_df$ts_start <- picked_df$ts_start %/% MS_TO_NS
# picked_df$ts_start <- picked_df$ts_start - origin
# picked_df$ts_end <- picked_df$ts_end %/% MS_TO_NS - origin
# picked_df$execution_time <- picked_df$execution_time / MS_TO_NS

# print(picked_df)

# Compare execution time and ts_end - ts_start
# Get ts_end - ts_start
# data_whole["ts_diff"] = (data_whole$ts_end - data_whole$ts_start) / MS_TO_NS
# plot(runtime_whole, col="red")
# lines(data_whole["ts_diff"], col="blue")

# Check stationary
df_ts <- ts(runtime_whole, start = 1)
plot(df_ts)
print(pacf(df_ts))
