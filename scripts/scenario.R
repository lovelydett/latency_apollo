# Analyse the driving scenario test results
# Yuting Xie
# 2022.7.6

# Main goals:
# 1. White noise test to prove some of the components are not senario irrelevent
# 2. For each scenario-aware component, analyse the correlation between info and execution time
# 3. For related components, analyse the correlation.

setwd("/Users/yuting/Codes/latency_apollo/scripts")
# setwd("/home/tt/Codes/latency_apollo/scripts")
# setwd("D:\\Codes\\latency_apollo\\scripts")

# Tools
source("./utils/constants.R")
source("./utils/load_data.R")
source("./utils/theme_publication.R")

# Packages
# library("stats")
library("ggplot2")
library("assert")
library("gridExtra")
library("cowplot")
library("ggstatsplot")
library("tidyr")
library("tseries")

# Turn E-Epress off
options(scipen = 999)

DATA_SETS = c('1', '2', '3', '4', '5', '6')

scenario_analyse <- function() {
    
}

white_noise_test <- function(df, task_name) {
    ts_et <- ts(df$execution_time, start = 1)
    test_result <- Box.test(ts_et, lag = 1, fitdf = 0)
    ts_et.acf <- acf(ts_et)
    ts_et.pacf <- pacf(ts_et)
    df_acf <- with(ts_et.acf, data.frame(lag, acf))
    df_pacf <- with(ts_et.pacf, data.frame(lag, acf))
    
    # Plot acf and pacf graph
    lim1 <- qnorm((1 + (1 - 0.05)) / 2) / sqrt(ts_et.acf$n.used)
    lim0 <- -1 * lim1
    g_acf <- ggplot(df_acf, mapping = aes(x = lag, y = acf)) +
        geom_bar(stat = "identity", colour = "#c554b6") +
        geom_hline(aes(yintercept = 0)) +
        # geom_segment(mapping = aes(xend = lag, yend = 0)) +
        geom_hline(aes(yintercept = lim1), linetype = 2, colour = "#386cb0") +
        geom_hline(aes(yintercept = lim0), linetype = 2, colour = "#386cb0") +
        labs(x = "Lag", y = "ACF") +
        scale_colour_Publication() +
        theme_Publication()
    lim1 <- qnorm((1 + (1 - 0.05)) / 2) / sqrt(ts_et.pacf$n.used)
    lim0 <- -1 * lim1
    g_pacf <- ggplot(df_pacf, mapping = aes(x = lag, y = acf)) +
        geom_bar(stat = "identity", colour = "#c554b6") +
        geom_hline(aes(yintercept = 0)) +
        # geom_segment(mapping = aes(xend = lag, yend = 0)) +
        geom_hline(aes(yintercept = lim1), linetype = 2, colour = "#386cb0") +
        geom_hline(aes(yintercept = lim0), linetype = 2, colour = "#386cb0") +
        labs(x = "Lag", y = "PACF") +
        scale_colour_Publication() +
        theme_Publication()

    g <- grid.arrange(g_acf, g_pacf, ncol = 2, nrow = 1, top = textGrob(task_name, gp = gpar(fontsize = 20, font = 3, fill = "white")))
    my_plot(g, name = paste0("ACF_PACF_", task_name), height = 10, width = 10)
}

driving_info_correlation <- function(df, task_name, coeff = 10, info_name = "info") {
    df <- df[400:700, ]
    df$id <- df$id - 400
    ###### Plot correlation graph ######
    g <- ggplot(df, aes(x = info, y = execution_time))
    g <- g + geom_point()
    g <- g + geom_smooth(method = "lm")
    g <- g + labs(title = task_name, x = info_name, y = "")
    g <- g + scale_colour_Publication() + theme_Publication()
    my_plot(g, name = paste0("Correlation_", task_name), height = 5, width = 5)

    ###### t-Test for correlation ######

    ###### Plot et-info graph ######
    # Align unit for info
    df$info <- df$info * coeff

    # Label row
    df <- pivot_longer(
        df,
        cols = c("info", "execution_time"),
        names_to = "line_type",
        values_to = "line_value"
    )
    g <- ggplot(df, aes(x = id, y = line_value, colour = line_type, shape = line_type))
    g <- g + geom_line(size = 1.5)
    g <- g + geom_point(size = 3)
    g <- g + scale_y_continuous(
        name = "Execution time (ms)",
        sec.axis = sec_axis(~. / coeff, name = info_name)
    )
    g <- g + labs(title = task_name, x = "Input message sequence")
    g <- g + scale_colour_Publication()
    g <- g + theme_Publication() + theme(legend.title = element_blank(), legend.position = "none")
    my_plot(g, name = paste0("Scenario_", task_name))

}

correlation_between_components <- function(df1, df2, task_name1, task_name2) {
    df1$ts_start <- df1$ts_start - df1$ts_start[1]
    df2$ts_start <- df2$ts_start - df2$ts_start[1]
    df1$component <- task_name1
    df2$component <- task_name2
    df <- rbind(df1, df2)
    g <- ggplot(df, aes(x = ts_start * NS_TO_MS, y = execution_time, colour = component)) +
        geom_point() +
        geom_line() +
        labs(x = "Time (ms)", y = "Execution time (ms)") +
        scale_colour_Publication() +
        theme_Publication() +
        theme(legend.title = element_blank())
    # my_plot(g, name = paste0("Corss_Correlation_", task_name1, "_", task_name2))

    # Cross correlation between components
    ccf_res <- ccf(df1$execution_time, df2$execution_time, lag = 10)
    df_ccf <- with(ccf_res, data.frame(lag, acf))
    lim1 <- qnorm((1 + (1 - 0.05)) / 2) / sqrt(ccf_res$n.used)
    lim0 <- -1 * lim1
    g_pacf <- ggplot(df_ccf, mapping = aes(x = lag, y = acf)) +
        geom_hline(aes(yintercept = 0)) +
        geom_segment(mapping = aes(xend = lag, yend = 0)) +
        geom_hline(aes(yintercept = lim1), linetype = 2, colour = "#386cb0") +
        geom_hline(aes(yintercept = lim0), linetype = 2, colour = "#386cb0") +
        labs(x = "Lag", y = "CCF") +
        scale_colour_Publication() +
        theme_Publication()
    my_plot(g_pacf, name = paste0("CCF_", task_name1, "_", task_name2), height = 5, width = 5)
}

df <- load_data("../data/dataset2_driving_info/2/trafficlight.csv", finish_only = TRUE, round = TRUE, is_smooth = FALSE)
driving_info_correlation(df, "TrafficlightDetection", coeff = 12, info_name = "Number of traffic lights")
# white_noise_test(df, "Detection")

# df2 <- load_data("../data/dataset2_driving_info/1/planning.csv", finish_only = TRUE, round = TRUE, is_smooth = FALSE)

# correlation_between_components(df, df2, "Prediction", "Planning")

dev.off()