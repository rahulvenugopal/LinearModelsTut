# Understand the commonalities and differences between
# Regression, Mediation and SEM

# Load necessary libraries
library(palmerpenguins)
library(dplyr)
library(lavaan)

# Clean data by removing NA values for this example
penguin_data_clean <- na.omit(penguins)

# Run the multiple regression model
# Predict body_mass_g using flipper_length_mm and bill_length_mm
regression_model <- lm(body_mass_g ~ flipper_length_mm + bill_length_mm,
                       data = penguin_data_clean)

# View the results
summary(regression_model)


# What and how much is being assessed


# Mediation Analysis ------------------------------------------------------

# Create a mediation model syntax
mediation_model_syntax <- '
  # Mediator (M) is flipper_length_mm
  flipper_length_mm ~ a*bill_length_mm

  # Outcome (Y) is body_mass_g
  body_mass_g ~ b*flipper_length_mm + c*bill_length_mm

  # Indirect and total effects
  indirect_effect := a*b
  total_effect := c + (a*b)
'

# Fit the mediation model
fit_mediation <- sem(mediation_model_syntax,
                     data = penguin_data_clean)

# View summary of results
summary(fit_mediation, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


# CFI and TLI are comparative fit indices. 
# Values close to 1 (like your 1.000) indicate a very good fit.
# They compare your model to a "null" or "baseline" model where all
# variables are uncorrelated.
# 
# RMSEA and SRMR: These are absolute fit indices.
# 
# flipper_length_mm ~ bll_lngth_ (a): This is Path A, the relationship between the independent variable (bll_lngth_, or bill length) and the mediator (flipper_length_mm). The Estimate (1.674) means for every 1 mm increase in bill length, flipper length increases by about 1.674 mm. The P-value (0.000) indicates this relationship is highly statistically significant.
# 
# body_mass_g ~ flppr_lng_ (b): This is Path B, the relationship between the mediator (flppr_lng_, or flipper length) and the dependent variable (body_mass_g). The Estimate (48.890) means for every 1 mm increase in flipper length, body mass increases by about 48.890 g, while controlling for bill length. The P-value (0.000) shows this is a highly significant effect.
# 
# body_mass_g ~ bll_lngth_ (c): This is Path C, the direct effect of the independent variable (bll_lngth_) on the dependent variable (body_mass_g). The Estimate (4.959) is the direct effect, but the P-value (0.339) is not statistically significant. This means that when you account for the mediating effect of flipper length, the direct relationship between bill length and body mass disappears. This is a sign of full mediation.

