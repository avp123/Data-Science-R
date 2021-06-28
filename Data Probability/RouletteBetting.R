# The following probabilities are based on betting on Roulette. There is a special bet which is on 5 out of 38 total pockets. Winning this bet pays $6 while losing this bet loses you $1

# Expected value of 1 bet
p <- 5/38
a <- 6
b <- -1
mu <- a*p + b*(1-p)
mu

# Standard error of payout for one bet
sigma <- abs(b-a) * sqrt(p*(1-p))
sigma

# Expected value of the AVERAGE payout over 500 bets
mu

# Standard error of the AVERAGE payout over 500 bets
n <- 500
sigma/sqrt(n)

# Expected value of the SUM payout over 500 bets
500 * mu

# Standard error of the SUM payout over 500 bets
sqrt(n) * sigma

# Probability of losing money over 500 bets
pnorm(0, n*mu, sqrt(n)*sigma)