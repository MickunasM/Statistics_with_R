# We will need ggplot2, lets load the tidyverse package
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
  geom_histogram(bins = 10) +
  labs(x = "Amount ($)",
       y = "Count",
       title = "Amir's deals are predicted to bring in around $6000/week
       in the next quarter") + 
  theme(plot.title = element_text(hjust = 0.5))







