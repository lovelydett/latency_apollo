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

# setwd("/Users/yuting/Codes/latency_apollo/scripts")
setwd("/home/tt/Codes/latency_apollo/scripts")
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
    df_lidar$sensor <- "LiDAR"
    df_radar$sensor <- "Radar"
    df <- rbind(df_cam, df_lidar, df_radar)
    g_lat <- ggplot(df, aes(x = id, y = lat, colour = sensor)) +
        geom_line(size = 1.5) + geom_point(size = 3) +
        labs(title = "Information vacuum period in ControlCommand", x = "ControlCommand sequence", y = "Period length (ms)") +
        coord_cartesian(ylim = c(1, 300)) +
        scale_colour_Publication() +  theme_Publication() + 
        theme(legend.title = element_blank())
    # my_plot(g_lat, "IVP")

    # From the view of EIL
    df <- df_whole
    df_cam <- df %>% select(ts_cam, lat_cam)
    df_lidar <- df %>% select(ts_lidar, lat_lidar)
    df_radar <- df %>% select(ts_radar, lat_radar)
    names(df_cam) <- c("ts", "lat")
    names(df_lidar) <- c("ts", "lat")
    names(df_radar) <- c("ts", "lat")
    df_cam$sensor <- "Camera"
    df_lidar$sensor <- "LiDAR"
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
    g_process <- ggplot(df, aes(x = id, y = lat, color = sensor)) +
        geom_line(size = 1.5) + geom_point(size = 3) +
        labs(title = "End-to-end processing time", x = "Sensory input sequence", y = "Latency (ms)") +
        coord_cartesian(ylim = c(1, 300)) +
        scale_colour_Publication() + theme_Publication() +
        theme(legend.title = element_blank())
    # my_plot(g_process, "E2E_Process")

    # Plot g_lat and g_process in one graph
    if (0) {
        g <- ggarrange(g_lat, g_process, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
        my_plot(g, "E2E", width = 20, height = 5)
    }

    ### Do moving average directly on EIL
    df_cam <- df_whole %>% select(ts_cam, lat_cam)
    df_lidar <- df_whole %>% select(ts_lidar, lat_lidar)
    df_radar <- df_whole %>% select(ts_radar, lat_radar)
    df_cam <- df_cam[!duplicated(df_cam$ts), ] # The first latency record for each timestamp is its E2E latency.
    df_lidar <- df_lidar[!duplicated(df_lidar$ts), ]
    df_radar <- df_radar[!duplicated(df_radar$ts), ]
    
    df_cam$lat_cam_pred <- movavg(df_cam$lat_cam, 3, "s")
    # print(head(df_cam))

    # plot(df_cam$lat_cam, df_cam$lat_cam_pred)
    # cor(df_cam$lat_cam, df_cam$lat_cam_pred, method = "pearson")
}

graph_builder <- function(channels) {
    
}

control_exit_latency("../data/dataset1/1/whole/control.csv")