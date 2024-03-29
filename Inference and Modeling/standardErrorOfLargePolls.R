library(tidyverse)

#Finding standard error given different values of p
N <- 100000
p <- seq(0.35, 0.65, length = 100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))

data.frame(p = p, SE = SE) %>%
  ggplot(aes(p, SE)) +
  geom_line()