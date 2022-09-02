# Analyse the GPU sharing in DNN related tasks
# Yuting Xie
# 2022.9.2

# setwd("/Users/yuting/Codes/latency_apollo/scripts")
setwd("/home/tt/Codes/latency_apollo/scripts")
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


analyse_gpu_sharing <- function(taskname) {
    filename <- paste0("../data/dataset3_gpu/", taskname, ".csv")
    df <- read.csv(filename, header = TRUE,  sep = ',',  stringsAsFactors = FALSE)
    print(head(df))
    for (i in 1 : nrow(df)) {
        if (i == 1) {
            
        }
    } 
}

analyse_gpu_sharing("perception")