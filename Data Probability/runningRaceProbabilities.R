library(gtools)
library(tidyverse)

# Ways to distribute 3 medals to 8 runners
medals <- permutations(8,3)
nrow(medals)

# Ways to distribute 3 medals among 3 runners from Jamaica
jamaica <- permutations(3,3)
nrow(jamaica)

# Probability all 3 medals are won by Jamaicans
nrow(jamaica)/nrow(medals)

# Monte Carlo simulations on vector of 8 runners in the race, probability all 3 winners are Jamaican
set.seed(1)

runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
B <- 10000

all_jamaica <- replicate(B, {
  results <- sample(runners, 3)
  all(results == "Jamaica")
})

mean(all_jamaica)