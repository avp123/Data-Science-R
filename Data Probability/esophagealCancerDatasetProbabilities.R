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

# Probability that a person who is a cancer case is in the highest alcohol usage group
high_alc_cases <- esoph %>%
  filter(alcgp == "120+") %>%
  pull(ncases) %>%
  sum()

p_case_high_alc <- high_alc_cases/all_cases
p_case_high_alc

# Probability that a person who is a cancer case is in the highest tobacco usage group
high_tob_cases <- esoph %>%
  filter(tobgp == "30+") %>%
  pull(ncases) %>%
  sum()

p_case_high_tob <- high_tob_cases/all_cases
p_case_high_tob

# Probability that a person who is a cancer case is in the highest tobacco usage group AND the highest alcohol usage group
high_alc_and_tob_cases <- esoph %>%
  filter(alcgp == "120+" & tobgp == "30+") %>%
  pull(ncases) %>%
  sum()

p_case_high_alc_and_tob <- high_alc_and_tob_cases/all_cases
p_case_high_alc_and_tob

# Probability that a person who is a cancer case is in the highest tobacco usage group OR the highest alcohol usage group
high_alc_or_tob_cases <- esoph %>%
  filter(alcgp == "120+" | tobgp == "30+") %>%
  pull(ncases) %>%
  sum()

p_case_high_alc_or_tob <- high_alc_or_tob_cases/all_cases
p_case_high_alc_or_tob

# OR use addition rule
p_case_either_highest <- p_case_high_alc + p_case_high_tob - p_case_high_alc_and_tob
p_case_either_highest

# Probability control is in highest alcohol group
high_alc_controls <- esoph %>%
  filter(alcgp == "120+") %>%
  pull(ncontrols) %>%
  sum()

p_control_high_alc <- high_alc_controls/all_controls
p_control_high_alc

# Difference in likelihood of cases being in highest alcohol group compared to controls
p_case_high_alc / p_control_high_alc

# Probability control is in highest tobacco group
high_tob_controls <- esoph %>%
  filter(tobgp == "30+") %>%
  pull(ncontrols) %>%
  sum()

p_control_high_tob <- high_tob_controls/all_controls
p_control_high_tob

# Probability that a person who is a control is in the highest tobacco usage group AND the highest alcohol usage group
high_alc_and_tob_controls <- esoph %>%
  filter(alcgp == "120+" & tobgp == "30+") %>%
  pull(ncontrols) %>%
  sum()

p_control_high_alc_and_tob <- high_alc_and_tob_controls/all_controls
p_control_high_alc_and_tob

# Probability that a person who is a control is in the highest tobacco usage group OR the highest alcohol usage group
high_alc_or_tob_controls <- esoph %>%
  filter(alcgp == "120+" | tobgp == "30+") %>%
  pull(ncontrols) %>%
  sum()

p_control_high_alc_or_tob <- high_alc_or_tob_controls/all_controls
p_control_high_alc_or_tob

# OR use addition rule
p_control_either_highest <- p_control_high_alc + p_control_high_tob - p_control_high_alc_and_tob
p_control_either_highest

# Difference in likelihood of cases being in highest alcohol or tobacco group compared to controls in the same groups
p_case_high_alc_or_tob / p_control_high_alc_or_tob

# Or using addition rule variables
p_case_either_highest / p_control_either_highest
