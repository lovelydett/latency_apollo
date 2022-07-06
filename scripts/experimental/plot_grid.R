# Arrange multiple plots in one graph
# Yuting Xie
# 2022.7.6

library("ggplot2")
library("cowplot")

do_job <- function() {
    c1 <- c(1:100)
    c2 <- c(-100:-1)
    df <- data.frame(c1, c2)
    # g1 <- ggplot(df, aes(x = c1, y = c2))
    # g1 <- g1 + geom_line()
    plot(rnorm(50), rnorm(50))
}


# do_job()
plot(rnorm(50), rnorm(50))
dev.off()