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

