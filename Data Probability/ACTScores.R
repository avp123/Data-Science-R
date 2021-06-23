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

# Creating Z scores from act_scores and finding probability that z_score is greater than 2 standard deviation above the mean
z_scores <- (act_scores - mean(act_scores))/sd(act_scores)
mean(z_scores > 2)

# Finding Act score 2 standard deviations above the mean
2*sd(act_scores) + mean(act_scores)

# Finding the 97.5th percentile of act_scores using qnorm()
qnorm(.975, mean(act_scores), sd(act_scores))

# Finding minimum integer score so that probability of the score is at least .95
cdf <- sapply(1:36, function (x){
  mean(act_scores <= x)
})
min(which(cdf >= .95))

# Expected 95th percentile of ACT scores given mean of 20.9 and sd of 5.7
qnorm(.95, 20.9, 5.7)

# Using quantiles to determine the percentile the score of 26 is in
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
names(sample_quantiles[max(which(sample_quantiles < 26))])

# QQ plot showing theoretical_quantiles on the x axis and sample_quantiles on the y axis with line of best fit
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
theoretical_quantiles <- qnorm(p, 20.9, 5.7)
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()