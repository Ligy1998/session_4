pacman::p_load(tidyverse)

# prior distributions -----------------------------------------------------

## uniform distribution 

# create density distribution
x_min <- 0
x_max <- 1
range <- seq(x_min, x_max, length.out = 100) # sample space
d <- dunif(range, min = x_min, max = x_max) # densities
UNI <- data.frame(range, d)
#All probability densities are 1 because the uniform distribution used has a constant probability density 
#across the entire range, reflecting the property of a uniform distribution
# Plot
ggplot(UNI, aes(x = range, y = d)) +
  geom_line(linewidth = 2) +
  labs(x = "x", 
       y = "Density") +
  theme_minimal()



## normal distribution 

# create density distributions
x_min <- -5
x_max <- 5
range <- seq(x_min, x_max, length.out = 100) # range
d <- dnorm(range, mean = 0, sd = 1) # densities
NORM <- data.frame(range, d)

# plot 
ggplot(NORM, aes(x = range, y = d)) +
  geom_line(linewidth = 2) +
  labs(x = "x", 
       y = "Density") +
  theme_minimal()


## beta distribution

# create density distributions
range <- seq(0, 1, length.out = 100)
d <- dbeta(range, shape1 = 3, shape2 = 2)
BETA <- data.frame(range, d)

# plot
ggplot(BETA, aes(x = range, y = d)) +
  geom_line(size = 2) +
  labs(x = "x", 
       y = "Density") +
  theme_minimal()

## exponential distribution

# create density distributions
x_min <- 0
x_max <- 10 
range <- seq(x_min, x_max, length.out = 100)
d <- dexp(range, rate = 1)
EXP <- data.frame(range, d)

# plot
ggplot(EXP, aes(x = range, y = d)) +
  geom_line(size = 2) +
  labs(x = "x", 
       y = "Density") +
  theme_minimal()


## gamma distribution 

# create density distributions
x_min <- 0
x_max <- 10
range <- seq(x_min, x_max, length.out = 100)
d <- dgamma(range, shape = 2, rate = 1)
GAMMA <- data.frame(range, d)

# plot
ggplot(GAMMA, aes(x = range, y = d)) +
  geom_line(size = 2) +
  labs(x = "x", 
       y = "Density") +
  theme_minimal()

# Prior predictive simulation --------------------------------------------------

# specify prior 
a <- 5
b <- 2

theta <- seq(0,1, length.out = 1e3) 
d <- dbeta(theta,shape1 = a, shape2 = b)
summary <- data.frame(theta, d) #theta在beta型的概率密度分布(其实就是给x轴换成theta)

ggplot(summary, aes(x = theta, y = d)) +
  geom_line(size = 1, linetype = "dashed") +
  labs(x = expression(theta), 
       y = "Density") +
  theme_minimal()

no <- 1e3 #choose 1000 sample
set.seed(312)
prior_smp <- data.frame(smp = rbeta(no, a, b))
# rbeta() function is used to generate random numbers from a beta distribution. 生成一千个theta
ggplot(summary) +
  geom_line(size = 1, linetype = "dashed", 
            aes(x = theta, y = d)) +
  geom_density(data = prior_smp, aes(x = smp), 
               color = "#F8766D", size = 1) + 
  labs(x = expression(theta), 
       y = "Density") +
  theme_minimal()

preds <- data.frame(L =vector("numeric", nrow(prior_smp)))
#a data frame with a single column named "L" is created, 
#where the number of rows in the data frame matches the number of rows in the prior_smp data frame. 
#This empty data frame is then used to store the simulated data in the subsequent loop.
N <- 1e3 # The total number of trials in the Bernoulli process.

set.seed(832)
for (i in seq_along(prior_smp$smp)){ 
  
  preds[i, "L"] <- rbinom(n = 1, size = N, prob = prior_smp[i, "smp"])
#伯努利试验，二项试验是一系列独立的伯努利试验的重复进行，每次试验都有相同的成功概率。
#每个theta测试一千次，n是一千次里成功的次数,一共一千个theta
}
#you are simulating a single experiment with 100 trials, 
#and the resulting random number will represent the number of successful outcomes observed in that particular experiment. 
#The value of the generated random number will range from 0 to 100, indicating the count of successes.

preds %>% ggplot(aes(x=L)) + 
  geom_histogram(fill = "#F8766D", color = "#F8766D", 
                 alpha = .5, bins = 100) + 
  scale_x_continuous(limits = c(0,N), breaks = seq(0,N,100)) + 
  labs(x = "Number of Simulated L out of 1000",
       y = "Simulated Frequency") + 
  theme_minimal()
#横坐标是一千次里成功的次数，纵坐标是这个成功次数出现的频率

# Testing the model ----------------------------------------------

# scale prior to probability 
summary$prior <- (summary$d)/sum(summary$d)
upper <- seq((1/1e3), 1, length.out = 1e3)
lower <- seq(0, (999/1e3), length.out = 1e3)
summary$prior <- pbeta(upper, 5, 2) - pbeta(lower,5,2)

ggplot(summary, aes(x = theta, y = prior)) +
  geom_line(size = 1, linetype = "dashed") +
  labs(x = expression(theta), 
       y = "Prior") + 
  theme_minimal()

# simulate data
sim_rides <- function(N, p){
  sample(c("L", "O"), size=N, replace=TRUE, prob=c(p, 1-p)) 
}

N <- 1e3
set.seed(12385)
obs <- sim_rides(N, p = .5)

# grid approximation of posterior

compute_post <- function(obs, summary){ 
  L <- sum(obs=="L")
  likelihood <- dbinom(L, N, prob = summary$theta)
  posterior <- likelihood*summary$prior
  posterior_norm <- posterior/sum(posterior)
  tibble(summary,lh=round(likelihood, 3), post=round(posterior_norm,3))
}
estimation <- compute_post(obs, summary)

# Check results
estimation %>% 
  pivot_longer(cols = c(prior,post), 
               names_to = "type", 
               values_to = "probability") %>% 
  ggplot(aes(x=theta, y = probability, color = type, linetype = type)) + 
  geom_line(size = 1) + 
  theme_minimal() + 
  labs(x = "Theta", 
       y = "Probability", 
       color = "Probability",
       linetype = "Probability")

# posterior predictive check ----------------------------------------------

estimation %>% 
  ggplot(aes(x=theta, y = post)) + 
  geom_line(size = 1, linetype = "dashed") + 
  labs(x = expression(theta), 
       y = "Probability") + 
  theme_minimal()

set.seed(123461)
post_smp <- data.frame(smp = sample(estimation$theta, 1e3, prob = estimation$post, replace = TRUE))

ggplot(post_smp, aes(x=smp)) +
  geom_histogram(fill = "#F8766D", color = "#F8766D", alpha = .5, bins = 500) + 
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.1)) + 
  labs(x = expression(theta), 
       y = "Probability") + 
  theme_minimal()

post_preds <- data.frame(L =vector("numeric", nrow(post_smp)))

N <- 1e3
for (i in seq_along(prior_smp$smp)){ 
  
  post_preds[i, "L"] <- rbinom(n = 1, size = N, prob = post_smp[i, "smp"])
  
}

post_preds %>% ggplot(aes(x=L)) + 
  geom_histogram(fill = "#F8766D", color = "#F8766D", alpha = .5, bins = 100) + 
  scale_x_continuous(limits = c(0,N), breaks = seq(0,N,100)) + 
  labs(x = "Number of Simulated L out of 1000",
       y = "Simulated Frequency") + 
  theme_minimal()