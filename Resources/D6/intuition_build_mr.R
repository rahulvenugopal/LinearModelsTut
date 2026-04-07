# Load libraries
library(tidyverse)
library(palmerpenguins)
library(lme4)
library(emmeans)
library(ggeffects)
library(easystats)

library(car)
library(sjPlot)

# Get data
data <- penguins %>% 
  select(body_mass_g, flipper_length_mm, bill_length_mm, species, sex) %>% 
  drop_na()


# Model 1 | Two continuous predictors -------------------------------------

model1 <- lm(data = data,
             body_mass_g ~ flipper_length_mm + bill_length_mm)
summary(model1)

# Concept of partial effects or holding other variables constant
# Credit stealing - A penguin with a very long bill is highly likely to also be
# a very large penguin overall, which means it probably also has very long 
# flippers.

# The bill is stealing credit from the flipper. The model sees a big bill, 
# notices a heavy penguin, and gives the bill all the credit, ignoring the 
# giant flippers attached to the same bird.

# Frisch-Waugh-Lovell theorem (or "partialing out").

# Looking strictly at the variations in bill length that cannot possibly be 
# explained by flipper length.

# Multicollinearity - P-values skyrocket, Coefficients become unstable,
# Loss of Meaning

# Check for multicollinearity | Variance inflation Factor
# values 5 to 10 is still okayish
vif(model1) # VIF 1 means zero correlation

# get a report
report(model1)

# Plot the coefficients
plot_model(model1, type = "est", show.values = TRUE)

# Check health of the model
png("my_model_dashboard.png", width = 1200, height = 1000, res = 150)
plot(check_model(model1))
dev.off()

