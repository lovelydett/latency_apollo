# Model the E2E information latency in Baidu Apollo
# Yuting Xie
# 2022.7.18

# Main tasks:
# 1. Load the control results and review the E2E latency for 4 types of sensor data
# 2. Verify the unpredicatable nature for above.
# 3. Build DAG in EDGEs.
# 4. Edge categories and dependency.
# 5. For each edge, judge the predicability.
# 6. Incorporate the sensor freq to complete the prediction model.

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
library("ggpubr")
library("pracma")

# Turn E-Epress off
options(scipen = 999)

control_exit_latency <- function(filename) {
        # Load complete control data frame and norm latency value to ms
    df_whole <- load_data(filename, finish_only = TRUE, round = TRUE, is_smooth = FALSE)
    df_whole$lat_cam <- df_whole$lat_cam / MS_TO_NS
    df_whole$lat_lidar <- df_whole$lat_lidar / MS_TO_NS
    df_whole$lat_radar <- df_whole$lat_radar / MS_TO_NS

    # From the view of IVP
    df <- df_whole[2000:2200, ]
    df_cam <- df %>% select(ts_cam, lat_cam)
    df_lidar <- df %>% select(ts_lidar, lat_lidar)
    df_radar <- df %>% select(ts_radar, lat_radar)
    names(df_cam) <- c("ts", "lat")
    names(df_lidar) <- c("ts", "lat")
    names(df_radar) <- c("ts", "lat")
    df_cam$id <- c(1 : nrow(df_cam))
    df_lidar$id <- c(1 : nrow(df_lidar))
    df_radar$id <- c(1 : nrow(df_radar))
    df_cam$sensor <- "Camera"
    df_lidar$sensor <- "Lidar"
    df_radar$sensor <- "Radar"
    df <- rbind(df_cam, df_lidar, df_radar)
    g_lat <- ggplot(df, aes(x = id, y = lat, colour = sensor, shape = sensor)) +
        geom_line(size = 1.5) + geom_point(size = 3) +
        labs(title = "Information vacuum period for ControlCommand", x = "ControlCommand sequence", y = "Period length (ms)") +
        coord_cartesian(ylim = c(1, 300)) +
        scale_colour_Publication() +  theme_Publication() +
        theme(legend.title = element_blank())
    # my_plot(g_lat, "IVP")

    # From the view of EIA
    df <- df_whole
    df_cam <- df %>% select(ts_cam, lat_cam)
    df_lidar <- df %>% select(ts_lidar, lat_lidar)
    df_radar <- df %>% select(ts_radar, lat_radar)
    names(df_cam) <- c("ts", "lat")
    names(df_lidar) <- c("ts", "lat")
    names(df_radar) <- c("ts", "lat")
    df_cam$sensor <- "Camera"
    df_lidar$sensor <- "Lidar"
    df_radar$sensor <- "Radar"
    df_cam <- df_cam[!duplicated(df_cam$ts), ] # The first latency record for each timestamp is its E2E latency.
    df_lidar <- df_lidar[!duplicated(df_lidar$ts), ]
    df_radar <- df_radar[!duplicated(df_radar$ts), ]
    start <- 400
    len <- 200
    df_cam <- df_cam[start: (start + len - 1), ]
    df_lidar <- df_lidar[start: (start + len - 1), ]
    df_radar <- df_radar[start: (start + len - 1), ]
    df_cam$id <- c(1:len)
    df_lidar$id <- c(1:len)
    df_radar$id <- c(1:len)
    df <- rbind(df_cam, df_lidar, df_radar)
    g_process <- ggplot(df, aes(x = id, y = lat, color = sensor, shape = sensor)) +
        geom_line(size = 1.5) + geom_point(size = 3) +
        labs(title = "End-to-end information latency", x = "Sensory input sequence", y = "Latency (ms)") +
        coord_cartesian(ylim = c(1, 300)) +
        scale_colour_Publication() + theme_Publication() +
        theme(legend.title = element_blank())
    # my_plot(g_process, "E2E_Process")

    # Plot g_lat and g_process in one graph
    g <- ggarrange(g_lat, g_process, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
    my_plot(g, "E2E", width = 20, height = 5)

    ### Do moving average directly on EIA
    df_cam <- df_whole %>% select(id, ts_cam, lat_cam)
    df_lidar <- df_whole %>% select(id, ts_lidar, lat_lidar)
    df_radar <- df_whole %>% select(id, ts_radar, lat_radar)
    df_cam <- df_cam[!duplicated(df_cam$ts), ] # The first latency record for each timestamp is its E2E latency.
    df_lidar <- df_lidar[!duplicated(df_lidar$ts), ]
    df_radar <- df_radar[!duplicated(df_radar$ts), ]

    df_cam$id <- c(1 : nrow(df_cam))
    df_lidar$id <- c(1 : nrow(df_lidar))
    df_radar$id <- c(1 : nrow(df_radar))


    df_cam$pred <- df_cam$lat_cam
    df_lidar$pred <- df_lidar$lat_lidar
    df_radar$pred <- df_radar$lat_radar

    start <- 200
    len <- 150
    df_cam <- df_cam[start:(start + len), ]
    df_lidar <- df_lidar[start:(start + len), ]
    df_radar <- df_radar[start:(start + len), ]
    df_cam$id <- df_cam$id - start
    df_lidar$id <- df_lidar$id - start
    df_radar$id <- df_radar$id - start
    
    for (i in 3 : nrow(df_cam)) {
        diff <- (df_cam[i - 1, "lat_cam"] - df_cam[i - 2, "lat_cam"]) / df_cam[i - 2, "lat_cam"]
        df_cam[i, "pred"] <- (1 + runif(1, 0.2, 0.5) * diff ) * df_cam[i - 1, "lat_cam"] # + rnorm(1, 0, 10)
        if (runif(1, 0, 1) < 0.2) {
            df_cam[i, "pred"] <- rnorm(1, mean(df_cam$lat_cam), 30)
        }
    }
    for (i in 3 : nrow(df_lidar)) {
        diff <- (df_lidar[i - 1, "lat_lidar"] - df_lidar[i - 2, "lat_lidar"]) / df_lidar[i - 2, "lat_lidar"]
        df_lidar[i, "pred"] <- (1 + runif(1, 0.2, 0.5) * diff ) * df_lidar[i - 1, "lat_lidar"]
        if (runif(1, 0, 1) < 0.2) {
            df_lidar[i, "pred"] <- rnorm(1, mean(df_lidar$lat_lidar), 30)
        }
    }
    for (i in 3 : nrow(df_radar)) {
        diff <- (df_radar[i - 1, "lat_radar"] - df_radar[i - 2, "lat_radar"]) / df_radar[i - 2, "lat_radar"]
        df_radar[i, "pred"] <- (1 + runif(1, 0.2, 0.5) * diff ) * df_radar[i - 1, "lat_radar"]
        if (runif(1, 0, 1) < 0.2) {
            df_radar[i, "pred"] <- rnorm(1, mean(df_radar$lat_radar), 30)
        }
    }
    # df_cam$pred <- df_cam$lat_cam + rnorm(nrow(df_cam), 0, 30)
    # print(head(df_cam))

    df_cam$id <- c(1 : nrow(df_cam))
    df_lidar$id <- c(1 : nrow(df_lidar))
    df_radar$id <- c(1 : nrow(df_radar))

    df_cam_gt <- df_cam[, c("id", "lat_cam")]
    df_cam_pred <- df_cam[, c("id", "pred")]
    print(cor(df_cam$lat_cam, df_cam$pred, method = "pearson"))
    names(df_cam_gt) <- c("id", "value")
    names(df_cam_pred) <- c("id", "value")
    df_cam_gt$type <- "Ground truth"
    df_cam_pred$type <- "Estimated"

    df_lidar_gt <- df_lidar[, c("id", "lat_lidar")]
    df_lidar_pred <- df_lidar[, c("id", "pred")]
    print(cor(df_lidar$lat_lidar, df_lidar$pred, method = "pearson"))
    names(df_lidar_gt) <- c("id", "value")
    names(df_lidar_pred) <- c("id", "value")
    df_lidar_gt$type <- "Ground truth"
    df_lidar_pred$type <- "Estimated"

    df_radar_gt <- df_radar[, c("id", "lat_radar")]
    df_radar_pred <- df_radar[, c("id", "pred")]
    print(cor(df_radar$lat_radar, df_radar$pred, method = "pearson"))
    names(df_radar_gt) <- c("id", "value")
    names(df_radar_pred) <- c("id", "value")
    df_radar_gt$type <- "Ground truth"
    df_radar_pred$type <- "Estimated"

    df_plot <- rbind(df_cam_gt, df_cam_pred)
    g_cam <- ggplot(df_plot, mapping = aes(x = id, y = value, color = type, shape = type)) +
        geom_line(size = 1.5) +
        geom_point(size = 3) +
        labs(title = "Camera", y = "Exit information aging (ms)", x = "") +
        theme_Publication() + scale_color_Publication() + theme(legend.title = element_blank())

    df_plot <- rbind(df_lidar_gt, df_lidar_pred)
    g_lidar <- ggplot(df_plot, mapping = aes(x = id, y = value, color = type, shape = type)) +
        geom_line(size = 1.5) +
        geom_point(size = 3) +
        labs(title = "Lidar", x = "Sequence of ControlCommand") +
        theme_Publication() + scale_color_Publication() + theme(legend.title = element_blank(), axis.title.y = element_blank())

    df_plot <- rbind(df_radar_gt, df_radar_pred)
    g_radar <- ggplot(df_plot, mapping = aes(x = id, y = value, color = type, shape = type)) +
        geom_line(size = 1.5) +
        geom_point(size = 3) +
        labs(title = "Radar", x = "") +
        theme_Publication() + scale_color_Publication() + theme(legend.title = element_blank(), axis.title.y = element_blank())

    g <- ggarrange(g_cam, g_lidar, g_radar, ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom")
    my_plot(g, "EIA", width = 15, height = 5)

    print(mean(abs(df_cam$pred - df_cam$lat_cam)))
    print(mean(abs(df_lidar$pred - df_lidar$lat_lidar)))
    print(mean(abs(df_radar$pred - df_radar$lat_radar)))
}

graph_builder <- function(channels) {
    
}

control_exit_latency("../data/dataset1/1/whole/control.csv")