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
library("tidyr")

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

driving_info_correlation <- function(df, coeff = 10, info_name = "info") {
    # Align unit for info
    df$info <- df$info * coeff

    # Label row
    df <- pivot_longer(
        df,
        cols = c("info", "execution_time"),
        names_to = "line_type",
        values_to = "line_value"
    )
    g <- ggplot(df, aes(x = id, y = line_value, colour = line_type))
    g <- g + geom_line()
    g <- g + geom_point()
    g <- g + scale_y_continuous(
        name = "Execution time (ms)",
        sec.axis = sec_axis(~. / coeff, name = "Scenario Info")
    )
    g <- g + scale_color_hue(labels = c("execution_time" = "Execution time", "info" = info_name))
    g <- g + theme_Publication() + theme(legend.title = element_blank())
    my_plot(g, name = "Scenario")
}

df <- load_data("../data/dataset2_driving_info/2/planning.csv", finish_only = TRUE, round = TRUE, is_smooth = FALSE)
driving_info_correlation(df)

dev.off()