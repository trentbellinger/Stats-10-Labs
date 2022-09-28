## Exercise 1

# 1a
n <- 365
p <- 0.40

# 1b
binom_mean <- n*p
binom_mean

binom_var <- n*p*(1-p)
binom_var

binom_std <- sqrt(n*p*(1-p))
binom_std

# 1c
dbinom(145, size=n, prob=p)

# 1d
pbinom(175, size=n, prob=p) - pbinom(124, size=n, prob=p)

# 1e
mu <- 200
sigma <- 20
1 - pnorm(230, mean=mu, sd=sigma)
# or
pnorm(230, mean=mu, sd=sigma, lower.tail=FALSE)

## Exercise 2

pawnee <- read.csv('pawnee.csv')

# 2a

# We first create objects for common quantities we will use for this exercise. 
n <- 30 # The sample size 
N <- 541 # The population size 
M <- 1000 # Number of samples/repetitions 
# Create vectors to store the simulated proportions from each repetition. 
phats <- numeric(M) # for sample proportions 
# Set the seed for reproduceability 
set.seed(123) 
# Always set the seed OUTSIDE the for loop. 
# Now we start the loop. Let i cycle over the numbers 1 to 1000 (i.e., iterate 1000 times). 
for(i in seq_len(M)){ 
  # The i-th iteration of the for loop represents a single repetition. 
  # Take a simple random sample of size n from the population of size N. 
  index <- sample(N, size = n) 
  # Save the random sample in the sample_i vector. 
  sample_i <- pawnee[index, ] 
  # Compute the proportion of the i-th sample of households with a new health issue. 
  phats[i] <- mean(sample_i$New_hlth_issue == "Y") 
}

hist(phats, prob=TRUE)
curve(dnorm(x, mean(phats), sd(phats)), add = TRUE)

# 2b
mean(phats)
sd(phats)
# The mean of the simulated sample proportion was about 29% with a standard deviation of about 8% (or 0.079).

# 2c
# Yes I believe the simulated distribution of sample proportions is approximately normal.
# There is a sufficient agreement with the mode and moderate agreement elsewhere. More importantly
# our simulated distribution is unimodal and symmetric.

# 2d
# bar{phat} = n * p / n = p = mean(phats)
# sd{phat} = sqrt(p * (1 - p) / n) = sqrt(mean(phats) * (1-mean(phats)) / n)
p = mean(phats)
p_sd = sqrt(p * (1 - p) / n)
p
p_sd

# We predicted ...
# These values are very close to our answers in part b. In particular our stheory tandard deviation is 
# within 0.004 of our empirical standard deviation.

## Exercise 3

# 3a

# We first create objects for common quantities we will use for this exercise. 
n <- 30 # The sample size 
N <- 541 # The population size 
M <- 1000 # Number of samples/repetitions 
# Create vectors to store the simulated proportions from each repetition. 
xbar <- numeric(M) # for sample means 
# Set the seed for reproduceability 
set.seed(123) 
# Always set the seed OUTSIDE the for loop. 
# Now we start the loop. Let i cycle over the numbers 1 to 1000 (i.e., iterate 1000 times). 
for(i in seq_len(M)){ 
  # The i-th iteration of the for loop represents a single repetition. 
  # Take a simple random sample of size n from the population of size N. 
  index <- sample(N, size = n) 
  # Save the random sample in the sample_i vector. 
  sample_i <- pawnee[index, ] 
  # Compute the proportion of the i-th sample of households with a new health issue. 
  xbar[i] <- mean(sample_i$Arsenic) 
}

# 3b
hist(xbar, prob=TRUE)
curve(dnorm(x, mean(xbar), sd(xbar)), add = TRUE)

# 3c
# We do not believe the simulated distribution for sample arsenic means is approximately normal.
# First, the distribution is right skewed. Second, there seems to be some distance between the mode of 
# our simulated distribution and the theoretical normal distribution. The theoretical normal distribution
# has significant chance of having attaining negative values. This is not possible with our simulated distribution.
hist(pawnee$Arsenic, breaks=35)
# The underlying distribution of arsenic is heavily skewed, so it should take more samples
# (larger n and N) for the Central Limit Theorem approximation to hold. This is why our
# result is different.

## Exercise 4

pawnee <- read.csv('pawnee.csv')

# 4a
head(pawnee)
dim(pawnee)

# 4b
set.seed(1337)
sample_indices <- sample(541, size=30)
pawnee_sample <- pawnee[sample_indices,]

sample_indices
head(pawnee_sample)

# 4c
mean(pawnee_sample$Arsenic)
mean(pawnee_sample$New_hlth_issue == 'Y')

# The mean arsenic level from our sample is 0.85 ppm.
# The proportion of households experiencing a major health issue in the sample is about 20%

# 4d
# The symbol from lecture for the mean arsenic level is xbar or bar(x).
# The symbol from lecture for the sample proportion is phat or hat(p).

# 4e
n <- 30

phat <- mean(pawnee_sample$New_hlth_issue == 'Y')
phat_sd <- sqrt(phat*(1-phat)/n)

# 90% CI
phat + phat_sd * qnorm(c(0.05, 0.95))

# 95% CI
phat + phat_sd * qnorm(c(0.025, 0.975))

# 99% CI
phat + phat_sd * qnorm(c(0.005, 0.995))

# 4f

# The 100% confidence interval for the population proportions is just all possible probabilities.
# This is the interval [0, 1].

# 4g

mean(pawnee$New_hlth_issue == 'Y')
# The true population mean of 0.292 falls within each of the confidence intervals from 4e.

# 4h

hist(pawnee$Arsenic, xlab='Arsenic [ppm]', main = 'Arsenic Histogram', breaks=45)


