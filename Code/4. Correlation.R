# Load the necessary package(s)
library(tidyverse)

# Load the necessary dataset(s)
world_happiness <- read_csv("Data/world_happiness.csv")

# ************************* Correlation ************************

# Correlations are relationships between numeric variables
# Most commonly used method is the Pearson product-moment correlation (r)
# The correlation coefficient is a numeric way of examining a relationship
# This number ranges between -1 and 1. The magnitude is strength and the + or -
# is the direction of the relationship


# We can visualize the correlations using geom_point(), 
# We can also add a trendline using geom_smooth with arguments method = 'lm' &
# se = 'FALSE'

# We can compute the correlation coefficient using the cor() function, if there
# are missing values, cor() function will return NA. To omit missing values 
# input the 'use' argument into the function, cor(use = "pairwise.complete.obs")


## Create a scatterplot of happiness_score vs. life_exp from world_happiness
ggplot(world_happiness, aes(x = life_exp, y = happiness_score)) +
  geom_point() +
  # Add a linear trendline to scatterplot
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() +
  labs(x = "Life Expectancy",
       y = "Happiness Score",
       title = "Happier people live longer") +
  theme(plot.title = element_text(hjust = 0.5))

## Calculate the correlation between life_exp and happiness_score
cor(world_happiness$life_exp, world_happiness$happiness_score)


# Correlation does not imply causation, x does not necessarily cause y, even if
# they are correlated, sometimes there are confounders to consider

# Scatterplot of gdp_per_cap and life_exp
ggplot(world_happiness, aes(x = gdp_per_cap, y = life_exp)) +
  geom_point()
# Calculate the correlation between gdp_per_cap and life_exp
cor(world_happiness$gdp_per_cap,
    world_happiness$life_exp)

# Although the correlation coefficient is 70%, it is not the best way to measure
# the relationship because the relationship is not linear, the variables are
# skewed, a transformation will be needed, for this lest use happiness_score
# instead of life_exp

# Create log_gdp_per_cap column
world_happiness <- world_happiness %>%
  mutate(log_gdp_per_cap = log(gdp_per_cap))

# Scatterplot of happiness_score vs. log_gdp_per_cap
ggplot(world_happiness, aes(x = log_gdp_per_cap, y = happiness_score)) +
  geom_point() +
  # Add a linear trendline to scatterplot
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() +
  labs(x = "GDP PER CAPITA",
       y = "Happiness Score",
       title = "Gross Domestic Product is associated with higher Happiness Scores ") +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate correlation
cor(world_happiness$log_gdp_per_cap, world_happiness$happiness_score)


