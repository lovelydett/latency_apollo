# Plot the ommunication overhead graph
# Yuting Xie
# 2022.8.8

setwd("/Users/yuting/Codes/latency_apollo/scripts")
# setwd("/home/tt/Codes/latency_apollo/scripts")
# setwd("D:\\Codes\\latency_apollo\\scripts")

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

plot_comm_overhead <- function() {
    df <- read.csv("../data/communication_overhead.csv", header = TRUE,  sep = ',',  stringsAsFactors = FALSE)

    df$mean <- df$mean / MS_TO_NS
    g <- ggplot(df, mapping = aes(x = message, y = mean, fill = message)) +
        geom_bar(stat='identity') +
        coord_flip() + # Flip the corrdinate to make horizontal bar chart
        theme_Publication() + scale_fill_Publication() +
        labs(x = "", y = "Serialization-transimission-deserialization time (ms)") +
        theme(axis.text = element_text(size = rel(0.7))) +
        theme(legend.position = "none")
    my_plot(g, name = "Comm_Overhead", height = 5, width = 15)
}

plot_comm_overhead()