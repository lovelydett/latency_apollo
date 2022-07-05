# Try plotting with ggplots.
# Yuting Xie
# 2022.7.5

setwd("/home/tt/Codes/latency_apollo/scripts")

source("utils/constants.R")
source("utils/load_data.R")
source("utils/theme_publication.R")

options(scipen = 999)
library("ggplot2")

tiff("test.tiff", units = "in", width = 12, height = 5, res = 300)
png("test.png", units = "in", width = 12, height = 5, res = 300)

df <- load_data("../data/dataset1/1/whole/prediction.csv", finish_only = TRUE, round = TRUE)

# Init ggplot
g <- ggplot(df, aes(x = id, y = execution_time, color = component))

# Add elements
g <- g + geom_line()
g <- g + geom_point()
g <- g + coord_cartesian(xlim = c(1, 1000), ylim = c(1, 70))
g <- g + labs(title = "Title", subtitle = "Sub title", x = "X label", y = "Y label")

# Change theme
# g <- g + theme(legend.position = "None")
# g <- g + scale_colour_brewer(palette = "Set2")

# Set X and Y breaks, and customize texts
g <- g + scale_x_continuous(breaks = seq(0, 1000, 100))
g <- g + scale_y_continuous(breaks = seq(0, 75, 10), , labels = function(x) {paste0(x, ' ms')})

# Theme for publication
g <- g + scale_colour_Publication() + theme_Publication()
plot(g)

dev.off()