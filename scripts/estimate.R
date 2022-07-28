# Build ARIMA model to estimate the component exectution times
# Yuting Xie
# 2022.7.27

# Main tasks:
# 1. Load dataset
# 2. ACF and PACF on 1-demesnional diff series.
# 3. Determine d, p, q in ARIMA(d, p, q).
# 4. Plot graph

# setwd("/Users/yuting/Codes/latency_apollo/scripts")
# setwd("/home/tt/Codes/latency_apollo/scripts")
setwd("D:\\Codes\\latency_apollo\\scripts")

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
    
    # Plot acf and pacf graph
    lim1 <- qnorm((1 + (1 - 0.05)) / 2) / sqrt(ts_et.acf$n.used)
    lim0 <- -1 * lim1
    g_acf <- ggplot(df_acf, mapping = aes(x = lag, y = acf)) +
        geom_bar(stat = "identity", fill = color) +
        geom_hline(aes(yintercept = 0)) +
        # geom_segment(mapping = aes(xend = lag, yend = 0)) +
        geom_hline(aes(yintercept = lim1), linetype = 2, colour = "#386cb0") +
        geom_hline(aes(yintercept = lim0), linetype = 2, colour = "#386cb0") +
        labs(x = "Lag", y = "ACF") +
        scale_fill_Publication() +
        theme_Publication()
    lim1 <- qnorm((1 + (1 - 0.05)) / 2) / sqrt(ts_et.pacf$n.used)
    lim0 <- -1 * lim1
    g_pacf <- ggplot(df_pacf, mapping = aes(x = lag, y = acf)) +
        geom_bar(stat = "identity", fill = color) +
        geom_hline(aes(yintercept = 0)) +
        # geom_segment(mapping = aes(xend = lag, yend = 0)) +
        geom_hline(aes(yintercept = lim1), linetype = 2, colour = "#386cb0") +
        geom_hline(aes(yintercept = lim0), linetype = 2, colour = "#386cb0") +
        labs(x = "Lag", y = "PACF") +
        scale_fill_Publication() +
        theme_Publication()

        g <- grid.arrange(g_acf, g_pacf, ncol = 2, nrow = 1)
        my_plot(g, name = paste0("ACF_PACF_", task_name), height = 3, width = 10)
        return(list(ts_et.acf, ts_et.pacf))
}

build_arima <- function(df, task, p, d, q) {
    df_ts <- df$execution_time
    n <- ndiffs(df_ts)
    print(paste0(task, " needs ", n, "-d diff"))
    if (n > 0) {
        df_ts <- diff(df_ts, n)
    }
    # plot(df_ts, type = "l", main = paste0(task, " after diff"))
    # acf(df_ts, main = "ACF", xlab = "Lag")
    model <- arima(as.data.frame(df_ts), order = c(p, d, q), method = "ML") # Max-likelihood
    print(paste0("p, d, q for ", task, "is ", p, ", ", d, ", ", q))
    print(model)
    qqnorm(model$residuals) # Test the model
}

acf_pacf <- function(dataset_dir) {
    if (1) {
        task <- "prediction"
        df <- load_data(paste0(dataset_dir, task, ".csv"), finish_only = FALSE, round = TRUE, is_smooth = FALSE)
        build_arima(df, task, p = 4, d = 1, q = 0)
        return()
    }
    i <- 1
    for (task in TASKS) {
        # df <- load_data(paste0(dataset_dir, task, ".csv"), finish_only = FALSE, round = TRUE, is_smooth = FALSE)
        ret <- get_acf_pacf(df, task, color = MY_COLORS[i])
        # determine_dpq_arima(df, task)
        # df_acf <- ret[1]
        # df_pacf <- ret[2]
        i <- i + 1
    }
}

# Main logic
acf_pacf("../data/dataset2_driving_info/1/")

