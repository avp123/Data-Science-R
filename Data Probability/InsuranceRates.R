# Load in libraries
library(tidyverse)
library(dslabs)

# Install package with datasets
install.packages("dslabs")

# Load dataset
data(death_prob)
head(death_prob)

### An insurance company offers a one-year term life insurance 
### policy that pays $150,000 in the event of death within one year. 
### The premium (annual cost) for this policy for a 50 year old female is $1,150. 
### Suppose that in the event of a claim, the company forfeits the premium and loses a total of $150,000, 
### and if there is no claim the company gains the premium amount of $1,150. 
### The company plans to sell 1,000 policies to this demographic.


# Probability 50 year old female in death_prob dies
p <- death_prob %>%
  filter(sex == "Female" & age == "50") %>%
  pull(prob)
p

# Expected net profit for company on 1 policy for a 50 year old female
a <- -150000
b <- 1150

mu <- a*p + b*(1-p)
mu

# Standard error for company on 1 policy for a 50 year old female
sigma <- abs(b-a) * sqrt(p*(1-p))
sigma

# Expected value for company on 1000 policies for 50 year old females
n <- 1000
n*mu

# Standard error for company on 1000 policies for 50 year old females
sqrt(n) * sigma

# Probability insurance company loses money using Central Limit Theorem
pnorm(0, n*mu, sqrt(n)*sigma)

# Now we will look at 50 year old Males

# Probability 50 year old male in death_prob dies
p_male <- death_prob %>%
  filter(sex == "Male" & age == "50") %>%
  pull(prob)
p_male

# Finding a premium value to make sure the expected profits from 1000 50 year old males is $700,000
p <- p_male
mu_sum <- 700000
n <- 1000
a <- -150000

b <- (mu_sum/n-a*p)/(1-p)
b

# Standard error for 1000 50 year old males using new preium rate
sigma_sum <- sqrt(n)*abs(b-a)*sqrt(p*(1-p))
sigma_sum

# Probability of losing money on 1000 policies to 50 year old males
pnorm(0, mu_sum, sigma_sum)

### We will now look at a scenario in which a lethal pandemic disease increases 
### the probability of death within 1 year for a 50 year old to .015. 
### Unable to predict the outbreak, the company has sold 1,000 $150,000 
### life insurance policies for $1,150.

# Expected value of company's profits over 1000 policies
p <- .015    # probability of claim
a <- -150000    # loss per claim
b <- 1150    # premium - profit when no claim
n <- 1000

exp_val <- n*(a*p + b*(1-p))
exp_val

# Standard error of expected value of company's profits over 1000 policies
se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
se

# Probability of company losing money
pnorm(0, exp_val, se)

# Probability of company losing more than 1 million dollars
pnorm(-1*10^6, exp_val, se)

# Finding lowest death probability in sequence from .01 to .03 jumping by .001 that results in chance of losing money exceeding 90%
p <- seq(.01, .03, .001)
a <- -150000    # loss per claim
b <- 1150    # premium - profit when no claim
n <- 1000

p_lose_money <- sapply(p, function(p){
  exp_val <- n*(a*p + b*(1-p))
  se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
  pnorm(0, exp_val, se)
})

data.frame(p, p_lose_money) %>%
  filter(p_lose_money > 0.9) %>%
  pull(p) %>%
  min()

# Finding lowest death probability in sequence from .01 to .03 jumping by .0025 that results in chance of losing 1 million dollars exceeding 90%
d <- seq(.01, .03, .0025)

d_lose_million <- sapply(d, function(d){
  exp_val <- n*(a*d + b*(1-d))
  se <- sqrt(n) * abs(b-a) * sqrt(d*(1-d))
  pnorm(-1*10^6, exp_val, se)
})

data.frame(d, d_lose_million) %>%
  filter(d_lose_million > 0.9) %>%
  pull(d) %>%
  min()

# Sampling model that determines profit/loss in millions if simulation is run 1000 times and probability of defaulting is .015
set.seed(25, sample.kind = "Rounding")

p <- .015
loss <- -150000
profit <- 1150
n <- 1000

outcomes <- sample(c(loss, profit), n, prob = c(p, 1-p), replace = TRUE)
sum(outcomes)/10^6

# Probability of losing 1 million or more dollars using Monte Carlo simulation that repeats 10000 times to simulate range of profits/losses over 1000 loans
set.seed(27, sample.kind = "Rounding")
B <- 10000

profits <- replicate(B, {
  outcomes <- sample(c(loss, profit), n, prob = c(p, 1-p), replace = TRUE)
  sum(outcomes)/10^6
})

mean(profits < -1)

### Now there is demand for life insurance because of the previously mentioned pandemic,
### so the company wants to find a premium cost that keeps the probability of losing money under 5%,
### and the death rate is still .015.

# Finding premium required for 5% chanc of losing money given 1,000 loans and loss per claim of -150,000 dollars
p <- .015
n <- 1000
l <- -150000
z <- qnorm(.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x

# Expected profit per policy at this premium rate
l*p + x*(1-p)

# Expected profit for 1000 policies at this premium rate
mu <- n*(l*p + x*(1-p))
mu

