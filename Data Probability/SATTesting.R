# The following probabilities deal with the old SAT. There was a -.25 penalty for incorrect answers and 1 point awarded for each correct answer. The test was 44 multiple choice questions with 5 answer choices for each question.

# Chance you guess a given question right on old SAT
p <- 1/5 # one correct choice of 5 options
p

# Expected value of points for guessing on a question
a <- 1
b <- -0.25
mu <- a*p + b*(1-p)
mu

# Expected value of points for guessing on all 44 questions
n <- 44
n*mu

# Standard error of guessing on all 44 questions
sigma <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
sigma

# Probability that a student scores 8+ points just by guessing using Central Limit Theorem
1-pnorm(8, mu, sigma)

# Probability that a student scores 8+ points just by guessing using Monte Carlo simulation on 10000 students
set.seed(21, sample.kind = "Rounding")
B <- 10000
n <- 44
p <- 0.2
tests <- replicate(B, {
  X <- sample(c(1, -0.25), n, replace = TRUE, prob = c(p, 1-p))
  sum(X)
})
mean(tests >= 8)




# The SAT has reduced the multiple choice options from 5 to 4 and getting an answer wrong is not penalized. The following code will show how this changed the probabilities of things occurring during the test.

# Expected value on the new SAT if you guess on every problem
p <- 1/4
a <- 1
b <- 0
n <- 44
mu <- n * a*p + b*(1-p)
mu

# Finding lowest value of p in given sequence such that probability of scoring above 35 is over 80%
p <- seq(0.25, 0.95, 0.05)
exp_val <- sapply(p, function(x){
  mu <- n * a*x + b*(1-x)
  sigma <- sqrt(n) * abs(b-a) * sqrt(x*(1-x))
  1-pnorm(35, mu, sigma)
})

min(p[which(exp_val > 0.8)])