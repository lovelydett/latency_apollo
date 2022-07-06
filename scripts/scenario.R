# Analyse the driving scenario test results
# Yuting Xie
# 2022.7.6

# Main goals:
# 1. White noise test to prove some of the components are not senario irrelevent
# 2. For each scenario-aware component, analyse the correlation between info and execution time
# 3. For related components, analyse the correlation.

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
library("cowplot")
library("ggstatsplot")

# Turn E-Epress off
options(scipen = 999)

DATA_SETS = c('1', '2', '3', '4', '5', '6')

scenario_analyse <- function() {
    
}

white_noise_test <- function(df) {
    ts_et <- ts(df$execution_time, start = 1)
    test_result <- Box.test(ts_et, lag = 1, fitdf = 0)
    print(test_result)
    ts_et.acf <- acf(ts_et)
    ts_et.pacf <- pacf(ts_et)
    # plot(ts_et)
}

driving_info_correlation <- function(df) {
    g <- ggplot(df, aes(x = id))
    g <- g + geom_line(aes(y = execution_time)) # execution time
    g <- g + geom_line(aes(y = info * 10)) # info
    g <- g + scale_y_continuous(
        name = "Execution time (ms)",
        sec.axis = sec_axis(~./10, name = "Scenario Info")
    )
    g <- g + scale_colour_Publication() + theme_Publication()
    my_plot(g, name = "Scenario2")
}

df <- load_data("../data/dataset2_driving_info/1/planning.csv", finish_only = TRUE, round = FALSE, is_smooth = FALSE)
driving_info_correlation(df)

dev.off()