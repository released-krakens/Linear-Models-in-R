### CASE: Working with continous explanatory variables

## Example: Reef fish abundance and rugosity ###
# Within a reef, 12  transects were laid randomly. 
# The number and species of fishes were recorded using 
# video transects (wih a SVS) also measuring rugosity
# and  recording the number of prey items for each of the sites.

## The aim is to explore how habitat complexity 
# (measured by rugosity) and prey abundance relate to fish abundance
# and whether the relationship changes with depth.

# install packages
install.packages("ggplot2")
install.packages("ppcor")
install.packages("tidyr")
install.packages("MASS")
install.packages("AER")
# Required libraries
library(ggplot2)
library(ppcor)
library(tidyr)
library(MASS)

# Setting a seed for reproducibility
set.seed(123)

# Generating the dataset

n_transects <- 12

data <- data.frame(
  Transect = 1:n_transects,
  Rugosity = runif(n_stransects, 0, 10), # Assuming rugosity is a continuous variable ranging from 0 to 10
  Fish_Abundance = rpois(n_transects, 20), # Assuming fish abundance is a count
  Prey_Abundance = rpois(n_transects, 50),  # Assuming prey abundance is also a count
  Depth = c(rep("10m",6),rep("15m",6))
)

# The simplest way to explore the relationship among the three continous variables
# is through a either a pairwise correlation or a partial correlation.
# Here the Spearman rank correlation is used because the data is not normally distributed. But it is
# assumed that variables have a monotonic relationship. Check this with scatterplots

# Computing the partial correlation between Rugosity and Fish_Abundance controlling for Prey_Abundance
pc_res <- pcor(data[, c("Rugosity", "Fish_Abundance", "Prey_Abundance")], method="spearman")
print(pc_res)

# A simple linear model  can also be used to explore the relationship between the variables, using ranked data to
# deal with non-normally distributed data or alternatively transforming it.


# Ranking the data
data_ranked <- data %>% 
  mutate(across(c(Rugosity, Fish_Abundance, Prey_Abundance), rank))

# Performing the regression analysis
model <- lm(Fish_Abundance ~ Rugosity + Prey_Abundance, data=data_ranked)
summary(model)

# But depth has not been accounted yet and it could affect any relation.
# A GLM can handle a response variable that does not come froma  normal distribution
# and can also account for the effect of categorical and continous explanatory variables. 
# Assuming fish abundance and prey abundance are counts, we can use a Poisson GLM

model_poisson<-glm(Fish_Abundance ~ Rugosity * Prey_Abundance * Depth, data=data, family=poisson)
summary(model_poisson)

# Checking overdispersion
library(AER)
dispersiontest(model_poisson)

# if the value is significantly greater than 1, then the data is iversispersed, indicating
# that the negative binomial link function might be best. If the value is significantly less than 1, then the data is underdispersed,
# indicating that the Poisson link function might be best. If the value is not significantly different from 1, then the Poisson link
# function is appropriate.

# If the data is overdispersed, then we can use the negative binomial link function
model_negbin <- glm.nb(Fish_Abundance ~ Rugosity * Prey_Abundance * Depth, data=data)
summary(model_negbin)

