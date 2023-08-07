# Measuring chance - can calculate probability of an event taking place by:
# number of ways an event can occur / number of total possible outcomes

# Sampling for a data frame - 'sample_n()' pulls out 1 row randomly
# Need to 'set.seed()' if we want to get the same random sample each time

# Sampling without replacement - sample the remaining data frame, not possible
# to sample the same row again as it already has been "removed"

# Sampling with replacement - sample the data frame with possibility of 
# sampling the same row twice (samples the same distribution)

# We will need dplyr, lets load the tidyverse package
library(tidyverse)

# ************************* Calculating Probabilities ************************

# Using the seller_1 dataset which assesses the performance of 1 seller, Amir:
amir_deals <- readRDS('Data/seller_1.rds')

# Count the deals for each product
amir_deals %>% 
  count(product)

# Calculate probability of picking a deal with each product
amir_deals %>%
  count(product) %>%
  mutate(prob = n/sum(n))


## Sampling deals:

# Set the random seed
set.seed(31)

# Take a sample of 5 deals without replacement.
amir_deals %>%
  sample_n(5)

# Sample 5 deals with replacement
amir_deals %>%
  sample_n(5, replace = TRUE)


# ************************* Discrete Distributions ************************

# Probability distribution - describes the probability of each possible outcome:
# a die has 6 sides, each side has 1/6th of being rolled. 
# The probability distribution is 1/6, 1/6, 1/6, 1/6, 1/6, 1/6, 1/6.

# Expected value of a distribution is the mean of a probability distribution:
# This is the sum of multiplying each value by its probability e.g.
# (1x1/6)+(2x1/6)+(3x1/6)+(4x1/6)+(5x1/6)+(6x1/6) = 3.5
# This type of distribution is a Discrete Probability Distribution as the 
# outcomes are discrete variables e.g. they can be counted

# Law of large numbers - as sample size increases, the sample mean will
# approach the theoretical mean = the more times a die is rolled, the closer
# the mean of rolling a fair die will be to 3.5

## Creating a probability distribution

# Create a dataframe we can use:
restaurant_groups <- data.frame (group_id  = c("A", "B", "C", "D", "E", "F",
                                               "G", "H", "I", "J"),
                  group_size = c(2, 4, 6, 2, 2, 2, 3, 2, 4, 2))

# Create a histogram of group_size
ggplot(restaurant_groups, aes(x = group_size)) +
  geom_histogram(bins = 5)

# Create probability distribution
size_distribution <- restaurant_groups %>%
  # Count number of each group size
  count(group_size) %>%
  # Calculate probability
  mutate(probability = n/sum(n))

size_distribution

# Calculate expected group size
expected_val <- sum(size_distribution$group_size *
                      size_distribution$probability)
expected_val

# Calculate probability of picking group of 4 or more
size_distribution %>%
  # Filter for groups of 4 or larger
  filter(group_size >= 4) %>%
  # Calculate prob_4_or_more by taking sum of probabilities
  summarise(prob_4_or_more = sum(probability))


# ************************* Continuous Distributions ************************

# 1 - model how long Amir will wait for a back-up using a continuous uniform 
# distribution, save his lowest possible wait time as min and his longest 
# possible wait time as max. Remember that back-ups happen every 30 minutes.
min <- 0
max <- 30

# Calculate probability of waiting less than 5 mins
prob_less_than_5 <- punif(5, min, max)
prob_less_than_5

# Calculate probability of waiting more than 5 mins
prob_greater_than_5 <- punif(5, min, max, lower.tail = FALSE)
prob_greater_than_5

# Calculate probability of waiting 10-20 mins
prob_between_10_and_20 <- punif(20, min, max) - punif(10, min, max)
prob_between_10_and_20


## Simulating Amir waiting 1000 times:

# Create a new dataframe called wait times
wait_times <- structure(list(simulation_nb = 1:1000), 
                        class = c("tbl_df", "tbl", "data.frame"), 
                        row.names = c(NA, -1000L))

# Set random seed
set.seed(334)

# Generate 1000 wait times between 0 and 30 mins, save in time column
wait_times %>%
  mutate(time = runif(1000, min = 0, max = 30)) %>%
  # Create a histogram of simulated times
  ggplot(aes(x = time)) +
  geom_histogram(bins = 30) +
  labs(x = "Time (minutes)", 
       y = "Count",
       title = "Distribution of simulated wait times for data back-up") +
  theme(plot.title = element_text(hjust = 0.5))



