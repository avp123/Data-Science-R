library(tidyverse)
library(dslabs)
data(heights)

# Filtering for only Male heights
x <- heights %>% filter(sex=="Male") %>% pull(height)

# Using cumulative distribution function on heights data set
F <- function(a) mean(x <= a)
1 - F(70)    # probability of male taller than 70 inches

F(80) - F(60)    # probability of male between 60 and 80 inches