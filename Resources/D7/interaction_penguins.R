# Load libraries
library(palmerpenguins)
library(ggplot2)

# Remove missing values for clean modeling
penguins_clean <- na.omit(penguins)

# ---------------------------------------------------------
# 1. Additive Model vs Interaction Model
# ---------------------------------------------------------

# Additive Model: Assumes the effect of flipper_length_mm on body_mass_g
# is the SAME for every species. 
# They share the exact same slope, just different intercepts.
additive_model <- lm(body_mass_g ~ flipper_length_mm + species, data = penguins_clean)
summary(additive_model)

# Interaction Model: Allows the effect of flipper_length_mm to DEPEND on the species.
# They get different intercepts AND different slopes.
# The * symbol expands to main effects AND the interaction (flipper_length_mm:species)
interaction_model <- lm(body_mass_g ~ flipper_length_mm * species, data = penguins_clean)
summary(interaction_model)

# Comparing Models: Does the interaction term improve our fit?
# We can use anova() to test if the more complex model is significantly better.
anova(additive_model, interaction_model)

# ---------------------------------------------------------
# 2. Interpreting the Interaction Coefficients
# ---------------------------------------------------------

# Look back at summary(interaction_model).
# Reference Level: The baseline species is Adelie.
# - flipper_length_mm: This is the slope for the Adelie penguins ONLY.
# - speciesChinstrap: Difference in intercept between Chinstrap and Adelie.
# - flipper_length_mm:speciesChinstrap: Difference in slope between Chinstrap and Adelie.
# To get the actual slope for Chinstrap, you add the baseline slope and this interaction coefficient.

# ---------------------------------------------------------
# 3. Plotting Interactions
# ---------------------------------------------------------

# Plotting the true interaction (different slopes)
# geom_smooth() fits separate linear models by default when color is mapped to a factor!

ggplot(penguins_clean, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  theme_minimal() +
  labs(
    title = "Interaction Effect: Body Mass ~ Flipper Length * Species",
    subtitle = "Notice how the slopes (steepness) differ slightly between species",
    x = "Flipper Length (mm)",
    y = "Body Mass (g)"
  ) +
  scale_color_manual(values = c("Adelie" = "darkorange", "Chinstrap" = "purple", "Gentoo" = "cyan4"))

# ---------------------------------------------------------
# 4. Homework Sandbox
# ---------------------------------------------------------

# 1. Fit a model predicting bill_length_mm with the interaction of bill_depth_mm * sex

# 2. To change the reference baseline (e.g., making Gentoo the reference for species),
# you can use the relevel() function:
# penguins_clean$species_new_ref <- relevel(penguins_clean$species, ref = "Gentoo")
# Now try rebuilding your models with the releveled factors!
