library(gtools)
library(tidyverse)

# Determining how many possible combinations there cane be if we choose 1 entree, 2 sides, and 1 drink from 6 entrees, 6 sides, and 2 drinks
nrow(combinations(6,1)) * nrow(combinations(6, 2)) * nrow(combinations(2, 1))

# Expanded previous information's total drinks abailable to 3 options
nrow(combinations(6,1)) * nrow(combinations(6, 2)) * nrow(combinations(3, 1))

# Expanded previous information's sides by allowing customer to select 3 sides, not 2
nrow(combinations(6,1)) * nrow(combinations(6, 3)) * nrow(combinations(3, 1))

# Creating function to find number of meal combinations given 1 entree from n options, 1 drink from 3 options, and 2 sides from 6 options
entree_choices <- function(n){
  nrow(combinations(n,1)) * nrow(combinations(6, 2)) * nrow(combinations(3, 1))
}

# Applies n = 1:12 to the function entree_choices
combos <- sapply(1:12, entree_choices)

# Finds the minimum value for n so that combos > 365
data.frame(entrees = 1:12, combo = combos) %>%
  filter(combos > 365) %>%
  min(.$entrees)


# Creating function to find number of meal combinations given 1 entree from 6 options, 1 drink from 3 options, and 2 sides from n options
side_choices <- function(n){
  nrow(combinations(6,1)) * nrow(combinations(n, 2)) * nrow(combinations(3, 1))
}

# Applies n = 2:12 to the function side_choices
combos <- sapply(2:12, side_choices)

# Finds the minimum value for n so that combos > 365
data.frame(sides = 2:12, combo = combos) %>%
  filter(combos > 365) %>%
  min(.$sides)