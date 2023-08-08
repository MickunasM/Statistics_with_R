# We will need ggplot2 and dplyr, lets load the tidyverse package
library(tidyverse)

# ************************* Normal Distribution ************************

# The normal distribution follows a bell curve:
# 1 - it is symmetrical, the left side is a mirror image of the right side
# 2 - the area under the curve = 1, just like any continuous distribution
# 3 - the prbability never hits 0 at the tails

# Areas under the normal distribution curve:
# 68% of data falls within 1 standard deviation
# 95% of data falls within 2 standard deviations
# 99.7% of data falls within 3 standard deviations

## Distribution of Amir's sales:
# Create a histogram with 10 bins to visualize the distribution of the amount
ggplot(amir_deals, aes(x = amount)) +
  geom_histogram(bins = 10) +
  labs(x = "Amount ($)",
       y = "Count",
       title = "On average, Amir's deals bring in around $5000/week") + 
  theme(plot.title = element_text(hjust = 0.5))

# Amir's deals follow a normal distribution, mean = $5000 and sd = $2000.
# What's the probability of deal worth < 7500
pnorm(7500, mean = 5000, sd = 2000)

# Probability of deal > 1000
pnorm(1000, mean = 5000, sd = 2000, lower.tail = FALSE)

# Probability of deal between 3000 and 7000
pnorm(7000, mean = 5000, sd = 2000) - pnorm(3000, mean = 5000, sd = 2000)

# Calculate amount that 75% of deals will be more than
qnorm(0.75, mean = 5000, sd = 2000, lower.tail = FALSE)


## Simulating sales under new market conditions
# next quarter, the worth of each sale will increase by 20% and the volatility, 
# or standard deviation, of each sale's worth will increase by 30% 

# Create a new df to simulate the new sales data:
new_sales <- structure(list(sale_num = 1:36), 
                       class = "data.frame", row.names = c(NA, -36L))

# Calculate new average amount
new_mean <- 5000 + (5000 * 0.2)

# Calculate new standard deviation
new_sd <- 2000 + (2000 * 0.3)

# Simulate 36 sales - 'rnorm()' generates random numbers
new_sales <- new_sales %>% 
  mutate(amount = rnorm(36, mean = new_mean, sd = new_sd))

# Create histogram with 10 bins
ggplot(new_sales, aes(x = amount)) +
  geom_histogram(bins = 10, colour = "red", fill = "blue") +
  labs(x = "Amount ($)",
       y = "Count",
       title = "Amir's deals are predicted to bring in around $6000/week
       in the next quarter") + 
  theme(plot.title = element_text(hjust = 0.5))


# ************************* Central Limit Theorem ************************

# The sampling distribution of a statistic becomes closer to the normal 
# distribution as the number of trials increases. This is also true for the sd
# & proportions.

# Lets use the amir_deals dataframe and see how the CLT works:
# Create a histogram of num_users
ggplot(amir_deals, aes(x = num_users))+
  geom_histogram(bins = 10)

# Set seed 
set.seed(104)

# Sample 20 num_users with replacement from amir_deals
sample(amir_deals$num_users, 20, replace = TRUE) %>%
  # Take mean
  mean()

# Repeat the above 100 times
sample_means <- replicate(100, sample(amir_deals$num_users, 
                                      size = 20, replace = TRUE) %>% mean())

# Create data frame for plotting
samples <- data.frame(mean = sample_means)

# Histogram of sample means
ggplot(samples, aes(x = mean)) +
  geom_histogram(bins = 10)
# Even though the distribution of num_users is not normal, the distribution of 
# its sample mean resembles the normal distribution.


## The mean of means:
# Estimate the mean by taking several random samples of deals, since this is 
# much easier than collecting data from everyone in the company.

# Lets load a dataset for all_deals from all the company's deals and see how
# Amir's deals compare

all_deals <- read_csv("Data/all_deals.csv")

# Set seed
set.seed(321)
# Take 30 samples of 20 values of num_users, take mean of each sample
sample_means <- replicate(30, sample(all_deals$num_users, 20) %>% mean())

# Calculate mean of sample_means
mean(sample_means)
# Calculate mean of num_users in amir_deals
mean(amir_deals$num_users)
# Amir's average number of users is very close to the overall average


# ************************* Poisson Distribution ************************

# This is the probability of some number of events occurring over a fixed 
# period of time e.g. 
# Probability of >5 animals adopted from an animal shelter per week
# This distribution is described by the lambda value which is the avg number of
# events per time interval

# Probability of a single value can be found using 'dpois()'
# Probability of less than or equal to can be found using 'ppois()'
# Probability of more than value can be found using 'ppois(lower.tail = FALSE)'
# Can take samples from a Poisson distribution using 'rpois()'

## Tracking lead responses:
# What's the probability that Amir responds to 5 leads in a day, given that he 
# responds to an average of 4?
dpois(5, lambda = 4)

# Amir's coworker responds to an average of 5.5 leads per day. What is the 
# probability that she answers 5 leads in a day?
dpois(5, lambda = 5.5)

# What's the probability that Amir responds to 2 or fewer leads in a day?
ppois(2, lambda = 4)

# What's the probability that Amir responds to more than 10 leads in a day?
ppois(10, lambda = 4, lower.tail = FALSE)


# ************************* Other Distribution ************************

# Exponential Distribution - probability of time between Poisson events e.g.
# probability of >1 day between animal adoptions. Also uses lambda value (rate)
# can be calculated using 'pexp()'

# (Student's) t-distribution - similar shape to a normal distribution but the 
# tails are thicker in a t-distribution meaning observations are more likely to
# fall further from the mean.
# Degrees of freedom - affect the thickness of the distribution's tails. Higher
# number of degrees of freedom mean the distribution starts to look more like a
# normal distribution because this = thinner tails and lower sd.

# Log-normal distribution - variables whose logarithm is normally distributed,
# this results in distributions that are skewed. 


## Modeling time between leads:
# What's the probability it takes Amir less than an hour to respond to a lead,
# when on average in takes 2.5 hours for him to respond?
pexp(1, rate = 0.4) # rate is equal to 1 divided by the average time.

# Probability response takes > 4 hours
pexp(4, rate = 0.4, lower.tail = FALSE)

# Probability response takes 3-4 hours
pexp(4, rate = 0.4) - pexp(3, rate = 0.4)














