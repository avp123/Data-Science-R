head(esoph)
library(tidyverse)

# Groups in the study
nrow(esoph)

# Number of cases
all_cases <- sum(esoph$ncases)
all_cases

# Number of controls
all_controls <- sum(esoph$ncontrols)
all_controls

# Probability subject in highest alcohol consumption group is a cancer case
esoph %>%
  filter(alcgp == "120+") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)

# Probability subject in lowest alcohol consumption group is a cancer case
esoph %>%
  filter(alcgp == "0-39g/day") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)

# Probability that a person smokes 10g or more a day given that they are a cancer case
tob_cases <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncases) %>%
  sum()

tob_cases/all_cases

# Probability that a person smokes 10g or more a day given that they are a control (not a cancer case)
tob_controls <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncontrols) %>%
  sum()

tob_controls/all_controls

