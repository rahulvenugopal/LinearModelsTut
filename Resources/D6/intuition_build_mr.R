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

# Bring in the intuition of regressing out -------------------------

library(palmerpenguins)
library(ggplot2)
library(dplyr)
library(patchwork)

# 1. Clean the dataset (remove rows with missing values)
df <- penguins %>% 
  tidyr::drop_na(body_mass_g, flipper_length_mm, bill_length_mm)

# Single predictor
plot_naive <- ggplot(df, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1.2) +
  labs(title = "1. Raw Relationship",
       subtitle = "Body Mass ~ Flipper Length",
       x = "Raw Flipper Length (mm)",
       y = "Raw Body Mass (g)") +
  theme_minimal()

# 1. Filter out Flipper Length from Body Mass (Outcome)
model_mass <- lm(body_mass_g ~ flipper_length_mm, data = df)
df$res_mass <- residuals(model_mass) # The pure mass

# 2. Filter out Flipper Length from Bill Length (Predictor)
model_bill <- lm(bill_length_mm ~ flipper_length_mm, data = df)
df$res_bill <- residuals(model_bill) # The pure bill length

# Step 3: Regress the Residuals on the Residuals
final_model <- lm(res_mass ~ res_bill, data = df)

# View the results
summary(final_model)

# 1. Filter out Bill Length from Body Mass (Outcome)
model_mass <- lm(body_mass_g ~ bill_length_mm, data = df)
df$res_mass <- residuals(model_mass) # The pure mass

# 2. Filter out Bill Length from Flipper Length (Predictor)
model_flipper <- lm(flipper_length_mm ~ bill_length_mm, data = df)
df$res_flipper <- residuals(model_flipper) # The pure flipper length

# Step 3: Regress the Residuals on the Residuals
final_model <- lm(res_mass ~ res_flipper, data = df)

# View the results
summary(final_model)
