# Lets work with the food consumption dataset, which contains information 
# about the kilograms of food consumed per person per year in each country,
# and also the carbon footprint of that food category, per person per year

# We will need dplyr, lets load the tidyverse package
library(tidyverse)

# ************** Mean and median************************************

# Calculate the mean and median of kilograms of food consumed per person 
# per year for Belgium and USA:

# Load the food_consumption dataset:
food_consumption <- readRDS('Data/food_consumption.rds')

# Filter for Belgium
belgium_consumption <- food_consumption %>%
  filter(country == "Belgium")

# Filter for USA
usa_consumption <- food_consumption %>%
  filter(country == "USA")

# Calculate mean and median consumption in Belgium
mean(belgium_consumption$consumption)
median(belgium_consumption$consumption)

# Calculate mean and median consumption in USA
mean(usa_consumption$consumption)
median(usa_consumption$consumption)

# Calculate the mean and median of the kilograms of food consumed per person 
# per year in each country:

food_consumption %>%
  # Filter for Belgium and USA
  filter(country %in% c("Belgium", "USA")) %>%
  # Group by country
  group_by(country) %>%
  # Get mean_consumption and median_consumption
  summarise(mean_consumption = mean(consumption),
            median_consumption = median(consumption))

# Filter food_consumption to get the rows where food_category is "rice".
# Create a histogram using ggplot2 of co2_emission for rice.

food_consumption %>%
  # Filter for rice food category
  filter(food_category == "rice") %>%
  # Create histogram of co2_emission
  ggplot(aes(x = co2_emission)) +
  geom_histogram() +
  labs(x = "Co2 Emissions (kg/person/year)",
       y = "Count",
       title = "Co2 emissions associated with consumption of rice",
       subtitle = "Generally below 50kg of Co2 per person per year")


# Summarize the data to get the mean and median of co2_emission, calling them 
# mean_co2 and median_co2.
food_consumption %>%
  # Filter for rice food category
  filter(food_category == "rice") %>% 
  # Get mean_co2 and median_co2
  summarize(mean_co2 = mean(co2_emission),
            median_co2 = median(co2_emission))


# ************** Measures of Spread************************************
# How close or spread apart the data points are, measures of spread:
# Variance - measures the avg distance from each data point to the data's mean
# Standard Deviation - how dispersed the data is in relation to the mean
# Quartiles - split data into 4 equal parts = range of values in each quartile
# Quantiles (percentiles) - split data into 5 or 10 pieces = same as quartile
# Interquartile range (IQR) - distance between 25th and 75th percentile

# Outliers - a data-point < Q1 - 1.5 * IQR or a data-point > Q3 + 1.5 * IQR

# Boxplots - the boxes represent quartiles: 
# The bottom of the box is the first quartile
# The top of the box is the third quartile
# The middle line is the second quratile or the median

## Quartiles, quantiles, and quintiles:

# 1 - Calculate the quartiles of the co2_emission column of food_consumption.
quantile(food_consumption$co2_emission)

# 2 - Calculate the six quantiles that split up the data into 5 pieces 
# (quintiles) of the co2_emission column of food_consumption.
quantile(food_consumption$co2_emission, probs = seq(0, 1, 0.2))

# 3 - Calculate the deciles of co2_emission
quantile(food_consumption$co2_emission, probs = seq(0, 1, 0.1))


## Variance and standard deviation:

# 1 - Calculate the variance and standard deviation of co2_emission for each 
# food_category by grouping by and summarizing variance as var_co2 and 
# standard deviation as sd_co2.
# Calculate variance and sd of co2_emission for each food_category
food_consumption %>% 
  group_by(food_category) %>% 
  summarise(var_co2 = var(co2_emission),
            sd_co2 = sd(co2_emission))

# 2 - Plot a histogram of co2_emission for each food_category using facet_wrap()
# Plot food_consumption with co2_emission on x-axis
ggplot(food_consumption, aes(x = co2_emission)) +
  # Create a histogram
  geom_histogram() +
  # Create a separate sub-graph for each food_category
  facet_wrap(~ food_category) +
  labs(x = "Co2 Emissions (kg/person/year)",
       y = "Count",
       title = "Co2 Emissions per Food Category") + 
  theme(plot.title = element_text(hjust = 0.5))


## Finding outliers using IQR:

# Calculate total co2_emission per country: emissions_by_country
emissions_by_country <- food_consumption %>%
  group_by(country) %>%
  summarize(total_emission = sum(co2_emission))

emissions_by_country

# Compute the first and third quartiles and IQR of total_emission
q1 <- quantile(emissions_by_country$total_emission, 0.25)
q3 <- quantile(emissions_by_country$total_emission, 0.75)
iqr <- q3 - q1 

# Calculate the lower and upper cutoffs for outliers
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr

# Filter emissions_by_country to find outliers
emissions_by_country %>%
  filter(total_emission > upper | total_emission < lower)
