# Build ARIMA model to estimate the component exectution times
# Yuting Xie
# 2022.7.27

# Main tasks:
# 1. Load dataset
# 2. ACF and PACF on 1-demesnional diff series.
# 3. Determine d, p, q in ARIMA(d, p, q).
# 4. Plot graph

setwd("/Users/yuting/Codes/latency_apollo/scripts")
# setwd("/home/tt/Codes/latency_apollo/scripts")
# setwd("D:\\Codes\\latency_apollo\\scripts")

# Tools
source("./utils/constants.R")
source("./utils/load_data.R")
source("./utils/theme_publication.R")

# Library
library("dplyr")
library("ggplot2")
library("assert")
library("gridExtra")
library("cowplot")
library("ggstatsplot")
library("tidyr")
library("tseries")
library("forecast")

# Turn E-Epress off
options(scipen = 999)

# Get the ACF and PACF graph respectively for a specific df
get_acf_pacf <- function(df, task_name, color = "#c554b6") {
    ts_et <- ts(df$execution_time, start = 1)
    # test_result <- Box.test(ts_et, lag = 1, fitdf = 0)
    ts_et.acf <- acf(ts_et, plot = FALSE)
    ts_et.pacf <- pacf(ts_et, plot = FALSE)
    df_acf <- with(ts_et.acf, data.frame(lag, acf))
    df_pacf <- with(ts_et.pacf, data.frame(lag, acf))

    return(list(df_acf, df_pacf, ts_et.acf$n.used, ts_et.pacf$n.used))
}

build_arima <- function(df, task, p, d, q) {
    print(paste0("p, d, q for ", task, " is ", p, ", ", d, ", ", q))
    # df$execution_time <- (df$execution_time - min(df$execution_time)) / (max(df$execution_time) - min(df$execution_time))
    df_ts <- df$execution_time
    # n <- ndiffs(df_ts)
    # print(paste0(task, " needs ", n, "-d diff"))
    # if (n > 0) {
    #     df_ts <- diff(df_ts, n)
    # }
    # plot(df_ts, type = "l", main = paste0(task, " after diff"))
    # acf(df_ts, main = "ACF", xlab = "Lag")
    model <- arima(as.data.frame(df_ts), order = c(p, d, q), method = "CSS-ML") # Max-likelihood
    predicted <- df_ts + model$residuals
    # res <- qqnorm(model$residuals) #get quantiles

    return(list(id = c(1 : length(df_ts)), predicted = predicted, truth = df_ts, residuals = as.double(model$residuals))) # Test the model
}

fit_arima_example <- function() {
    dataset_dir <- "../data/dataset2_driving_info/1/"
    task <- "prediction"
    p <- 10
    d <- 1
    q <- 5
    df <- load_data(paste0(dataset_dir, task, ".csv"), finish_only = FALSE, round = TRUE, is_smooth = FALSE)
    res <- build_arima(df, task, p, d, q)
    res <- as.data.frame(res)
    print(names(res))
    res$predicted[res$predicted < 0] <- 10

    # 1. Plot correlation of predicted and truth values.
    g_correlation <- ggplot(res, mapping = aes(x = predicted, y = truth)) +
        geom_point() +
        geom_abline(slope = 1, intercept = 0, colour = "#8E0C24", size = 2) +
        coord_cartesian(xlim = c(0, 75), ylim = c(0, 75)) +
        scale_colour_Publication() +
        theme_Publication()
    # my_plot(g, name = paste0("R_predicted_truth_", task), height = 10, width = 10)
    # print(graphs[1])
    # g <- grid.arrange(graphs[[1]], graphs[[2]], graphs[[3]], graphs[[4]], ncol = 2, nrow = 2)
    # my_plot(g, name = "QQNORM", height = 10, width = 10)

    # 2. Plot acf / pacf for residuals to test the model.
    res$execution_time <- res$residuals
    ret <- get_acf_pacf(res, task_name = "prediction_residuals")
    df_acf <- ret[[1]]
    df_pacf <- ret[[2]]
    nused_acf <- ret[[3]]
    nused_pacf <- ret[[4]]

    lim1 <- qnorm((1 + (1 - 0.05)) / 2) / sqrt(nused_acf)
    lim0 <- -1 * lim1
    g_acf <- ggplot(df_acf, mapping = aes(x = lag, y = acf)) +
        geom_bar(stat = "identity", fill = "#c554b6") +
        geom_hline(aes(yintercept = 0), size = 1.5) +
        # geom_segment(mapping = aes(xend = lag, yend = 0)) +
        geom_hline(aes(yintercept = lim1), linetype = 2, colour = "#FFD800", size = 1.3) +
        geom_hline(aes(yintercept = lim0), linetype = 2, colour = "#FFD800", size = 1.3) +
        coord_cartesian(xlim = c(0, 20)) +
        labs(x = "Lag", y = "(P)ACF") +
        scale_fill_Publication() +
        theme_Publication()
    lim1 <- qnorm((1 + (1 - 0.05)) / 2) / sqrt(nused_pacf)
    lim0 <- -1 * lim1
    g_pacf <- ggplot(df_pacf, mapping = aes(x = lag, y = acf)) +
        geom_bar(stat = "identity", fill = "#c554b6") +
        geom_hline(aes(yintercept = 0), size = 1.5) +
        # geom_segment(mapping = aes(xend = lag, yend = 0)) +
        geom_hline(aes(yintercept = lim1), linetype = 2, colour = "#FFD800", size = 1.3) +
        geom_hline(aes(yintercept = lim0), linetype = 2, colour = "#FFD800", size = 1.3) +
        coord_cartesian(xlim = c(0, 20)) +
        labs(x = "Lag", y = "PACF") +
        scale_fill_Publication() +
        theme_Publication()
    # g_2 <- grid.arrange(g_acf, g_pacf, ncol = 2, nrow = 1)
    # my_plot(g, name = paste0("ACF_PACF_", "prediction_residuals"), height = 3, width = 10)

    # 3. plot residuals, predicted and truth series in one graph.
    tmp_data1 <- res[, c("id", "residuals")]
    tmp_data1$value <- tmp_data1$residuals
    tmp_data1$type <- "Residuals"
    tmp_data2 <- res[, c("id", "predicted")]
    tmp_data2$value <- tmp_data2$predicted * 2
    tmp_data2$type <- "Predicted"
    tmp_data3 <- res[, c("id", "truth")]
    tmp_data3$value <- tmp_data3$truth * 2
    tmp_data3$type <- "Ground truth"

    len <- 100
    tmp_data1 <- tmp_data1[1 : len, c("id", "value", "type")]
    tmp_data2 <- tmp_data2[1 : len, c("id", "value", "type")]
    tmp_data3 <- tmp_data3[1 : len, c("id", "value", "type")]

    tmp_data <- rbind(tmp_data2, tmp_data3)

    g_series <- ggplot(tmp_data, mapping = aes(x = id, y = value, color = type)) +
        geom_point(size = 2) +
        geom_line(size = 1) +
        geom_line(aes(group = id), color = "#8E0C24", size = 1) + # Mannualy add error bar
        scale_colour_Publication() +
        theme_Publication() +
        theme(legend.position = c(0.5, 0.7), legend.title = element_blank())
    # my_plot(g, name = paste0("Truth_Predicted_", task), height = 2, width = 6)

    # Arrange these figures in one figure.
    g <- grid.arrange(g_series, arrangeGrob(g_correlation, g_acf, g_pacf, ncol = 3))
    my_plot(g, name = paste0("ARIMA_Result_", task), height = 4, width = 8)
    
}

acf_pacf <- function(dataset_dir) {
    i <- 1
    for (task in TASKS) {
        df <- load_data(paste0(dataset_dir, task, ".csv"), finish_only = FALSE, round = TRUE, is_smooth = FALSE)
        ret <- get_acf_pacf(df, task, color = MY_COLORS[i])
        # determine_dpq_arima(df, task)
        # df_acf <- ret[1]
        # df_pacf <- ret[2]
        i <- i + 1
    }
}

# Main logic
# acf_pacf("../data/dataset2_driving_info/1/")
fit_arima_example()

