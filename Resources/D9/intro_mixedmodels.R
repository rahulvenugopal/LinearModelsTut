# INTRODUCTION TO LINEAR MIXED MODELS
# Based on: Our Coding Club (https://ourcodingclub.github.io/tutorials/mixed-models/)

# load libraries
library(tidyverse)  # For data wrangling and all ggplot2 visualizations
library(lme4)       # The core package for running linear mixed-effects models
library(ggeffects)  # For extracting model predictions to plot them cleanly
library(sjPlot)     # Great for visualizing random effects specifically
library(stargazer)  # For formatting model output into neat text/HTML/LaTeX tables

# Laod data
# Make sure the 'dragons.RData' file is in your working directory.
# We are looking at a fictional dataset where we want to know if dragon 
# body length affects their intelligence test scores. 
load("dragons.RData")

dragons <- dragons %>% 
  select(-X)

# Let's check the distribution of our response variable (Test Score)
ggplot(dragons, aes(x = testScore)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "white", alpha = 0.8) +
  labs(title = "Distribution of Dragon Test Scores",
       subtitle = "Checking for a roughly normal distribution",
       x = "Intelligence Test Score", 
       y = "Frequency") +
  theme_minimal()

# STANDARDISATION (Centering and Scaling)
# It centers the mean at 0 and sets the standard deviation to 1.
# This makes model convergence easier for the algorithm and puts all 
# explanatory variables on the same scale, making effect sizes directly comparable.
dragons$bodyLength2 <- scale(dragons$bodyLength,
                             center = TRUE,
                             scale = TRUE)

# Let us trash linear models (don't tell anyone outside CCS, ha ha ha )

# Let's first fit a basic linear model ignoring that dragons come from 
# different mountain ranges.
basic.lm <- lm(testScore ~ bodyLength2, data = dragons)
summary(basic.lm)

# Visualizing the simple (but flawed) linear relationship
ggplot(dragons, aes(x = bodyLength, y = testScore)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "darkred", fill = "pink") +
  labs(title = "Basic Linear Regression (Ignoring Mountain Ranges)",
       x = "Body Length (Original Scale)", 
       y = "Test Score") +
  theme_classic()
# Use the bodyLength2 as x and see what happens

# Predicted test scores and the residuals
lm_diagnostics <- data.frame(
  Fitted = fitted(basic.lm),
  Residuals = resid(basic.lm)
)

# Residuals vs Fitted Plot
ggplot(lm_diagnostics, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Q-Q Plot (Checking for normality of residuals)
ggplot(lm_diagnostics, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line(color = "red", linetype = "dashed") +
  labs(title = "Normal Q-Q Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# Our basic model assumes all data points are independent. BUT, our dragons 
# were collected from 8 specific mountain ranges. Dragons from the same 
# mountain might be more similar to each other than to dragons elsewhere.

# Seeing could be beleiving
ggplot(dragons, aes(x = mountainRange,
                    y = testScore,
                    fill = mountainRange)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  labs(title = "Test Scores Vary Wildly by Mountain Range",
       subtitle = "This proves our data points are NOT independent!",
       x = "Mountain Range", 
       y = "Test Score") +
  theme_minimal()

# Faceted scatterplot showing the relationship per mountain
ggplot(dragons, aes(x = bodyLength,
                    y = testScore,
                    color = mountainRange)) + 
  geom_point(show.legend = FALSE) + 
  facet_wrap(~ mountainRange) + 
  labs(title = "Body Length vs Test Score per Mountain Range",
       x = "Body Length", 
       y = "Test Score") +
  theme_bw()

# Welcome Linear Mixed Models ---------------------------------------------
# Mixed models contain both FIXED effects (the things we care about testing, 
# like body length) and RANDOM effects (the grouping factors we want to 
# control for, like mountain range).

# MODEL 1: Random Intercept Model
# Syntax: (1|mountainRange)
# Meaning: We allow each mountain range to have its own baseline test score 
# (intercept), but we assume body length affects test scores the same way 
# (same slope) across all mountains.
mixed.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange), data = dragons)
summary(mixed.lmer)

# MODEL 2: Nested Random Intercepts
# Syntax: (1|mountainRange/site) OR (1|mountainRange) + (1|site)
# Meaning: If we sampled multiple specific sites within each mountain range, 
# we can account for this hierarchy.
mixed.lmer2 <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|site), data = dragons)
summary(mixed.lmer2)

# MODEL 3: Random Slope + Random Intercept Model
# Syntax: (1 + bodyLength2|mountainRange)
# Meaning: We allow each mountain range to have its own baseline test score AND 
# we allow the relationship (slope) between body length and test score to differ 
# depending on the mountain range.
mixed.ranslope <- lmer(testScore ~ bodyLength2 + (1 + bodyLength2|mountainRange), data = dragons)
summary(mixed.ranslope)

# Plotting Overall Fixed Effects Predictions
# ggpredict() calculates the predicted values based purely on our fixed effect
# (body length), holding the random effects constant.
pred.mm <- ggpredict(mixed.lmer2, terms = c("bodyLength2"))

ggplot(pred.mm) + 
  # Plot the overall fixed effect prediction line
  geom_line(aes(x = x, y = predicted), color = "black", linewidth = 1) + 
  # Add confidence interval ribbon
  geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
              fill = "lightgrey", alpha = 0.5) + 
  # Overlay the raw data colored by mountain range
  geom_point(data = dragons, aes(x = bodyLength2, y = testScore, colour = mountainRange), alpha = 0.6) + 
  labs(x = "Body Length (Standardized)", 
       y = "Test Score", 
       title = "Overall Fixed Effect of Body Length on Intelligence",
       subtitle = "After accounting for mountain/site variance, body length has no real effect") + 
  theme_minimal()

# Plotting Random Intercepts (Group Level Predictions)
# By setting type="re", we tell ggeffects to calculate the prediction line 
# specifically for each level of our random effect (mountain range).
random_pred <- ggpredict(mixed.lmer2,
                         terms = c("bodyLength2", "mountainRange"),
                         type = "random")

# We convert it to a standard ggplot object
plot(random_pred) + 
  labs(x = "Body Length (Standardized)", 
       y = "Test Score", 
       title = "Predicted Test Scores by Mountain Range (Random Intercepts)",
       color = "Mountain Range") + 
  theme_minimal() +
  theme(legend.position = "right")

# Visualizing the Random Effects Departure (Caterpillar Plot)
# This sjPlot function shows how much the intercept/slope of each mountain 
# deviates from the global average. If a dot crosses the 0 vertical line, 
# it means it is not significantly different from the global average.
plot_model(mixed.ranslope, type = "re", show.values = TRUE, 
           value.offset = 0.3, colors = "Set1") +
  labs(title = "Random Effects Variance by Mountain Range") +
  theme_minimal()

# Export as neat formatted tables
# Stargazer creates beautiful output tables. 
# You can change type="text" to "html" or "latex" when you are ready 
# to export this to a document or webpage.
stargazer(mixed.lmer, mixed.lmer2, mixed.ranslope, 
          type = "text", 
          digits = 3, 
          star.cutoffs = c(0.05, 0.01, 0.001), 
          digit.separator = "",
          title = "Comparison of Linear Mixed Models",
          column.labels = c("Random Intercept", "Nested Intercepts", "Random Slopes"))