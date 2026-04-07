# Day 6 - Multiple Regression Tutorial 2
# Continuous Y with two categorical X variables
# Model focus: body_mass_g ~ species + sex

# 1. Load libraries and data
library(tidyverse)
library(palmerpenguins)

penguins_cat <- penguins %>%
  select(body_mass_g, species, sex) %>%
  drop_na() %>%
  mutate(
    species = factor(species),
    sex = factor(sex)
  )

cat("Rows used:", nrow(penguins_cat), "\n")
cat("Reference level for species:", levels(penguins_cat$species)[1], "\n")
cat("Reference level for sex:", levels(penguins_cat$sex)[1], "\n\n")

# 2. Fit additive model
# Interpretation:
#   - species terms = differences from reference species (holding sex constant)
#   - sexmale term = male-female difference (holding species constant)
model_add <- lm(body_mass_g ~ species + sex, data = penguins_cat)
summary(model_add)

cat("\nANOVA table for additive model:\n")
print(anova(model_add))

# 3. Interaction model: does the sex gap differ by species?
model_interaction <- lm(body_mass_g ~ species * sex, data = penguins_cat)
summary(model_interaction)

cat("\nNested comparison (is interaction needed?):\n")
print(anova(model_add, model_interaction))

# 4. Predicted cell means (all species x sex combinations)
new_grid <- expand_grid(
  species = levels(penguins_cat$species),
  sex = levels(penguins_cat$sex)
)

new_grid$pred_mass_g <- predict(model_interaction, newdata = new_grid)

cat("\nPredicted means from interaction model:\n")
print(new_grid)

# 5. Observed means for cross-check
obs_means <- penguins_cat %>%
  summarize(mean_mass_g = mean(body_mass_g), .by = c(species, sex))

cat("\nObserved group means:\n")
print(obs_means)

# 6. Visuals
ggplot(penguins_cat, aes(x = species, y = body_mass_g, fill = sex)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 21) +
  geom_jitter(
    aes(color = sex),
    width = 0.15,
    alpha = 0.25,
    size = 1.2,
    show.legend = FALSE
  ) +
  labs(
    title = "Continuous Y with Two Categorical Predictors",
    subtitle = "Outcome: body_mass_g | Predictors: species + sex",
    x = "Species",
    y = "Body Mass (g)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

ggplot(new_grid, aes(x = species, y = pred_mass_g, group = sex, color = sex)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  labs(
    title = "Model-Predicted Means (Interaction Model)",
    x = "Species",
    y = "Predicted Body Mass (g)"
  ) +
  theme_minimal(base_size = 12)
