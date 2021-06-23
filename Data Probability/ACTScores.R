# ACT Score data set analysis: 2016 - 2018 data; 20.9 mean and 5.7 standard deviation. We are considering this data set to be continuous, not discrete.

# Calculated mean of normal distribution of 10000 tests with given mean and sd
set.seed(16, sample.kind = "Rounding")
act_scores <- rnorm(10000, 20.9, 5.7)
mean(act_scores)

# Standard deviation of act_scores
sd(act_scores)

# Number of ACT scores in act_scores greater than or equal to 36 (basically perfect scores)
sum(act_scores >= 36)

# Probability ACT score in act_scores is greater than 30
mean(act_scores > 30)

# Probability that ACT score in act_scores is less than or equal to 10
mean(act_scores <= 10)

# Using a sequence x and dnorm to determine a probability density function and plot x against that function
x <- 1:36
f_x <- dnorm(x, 20.9, 5.7)
data.frame(x, f_x) %>%
  ggplot(aes(x, f_x)) +
  geom_line()
