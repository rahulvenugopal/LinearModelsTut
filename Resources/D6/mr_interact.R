# Day 6 - Multiple Regression Tutorial 3
# Interaction Effects in Linear Models
# Goal: Learn when and how to model X1 * X2 terms using Palmer Penguins

# 1. Load libraries and data
library(tidyverse)
library(palmerpenguins)

penguins_int <- penguins %>%
  select(body_mass_g, flipper_length_mm, species, sex) %>%
  drop_na() %>%
  mutate(
    species = factor(species),
    sex = factor(sex)
  )

cat("Rows used:", nrow(penguins_int), "\n")
cat("Species levels:", paste(levels(penguins_int$species), collapse = ", "), "\n")
cat("Sex levels:", paste(levels(penguins_int$sex), collapse = ", "), "\n\n")

# 2. Continuous x Categorical interaction
# Question: Does the flipper_length -> body_mass relationship change by sex?
model_add_cc <- lm(body_mass_g ~ flipper_length_mm + sex, data = penguins_int)
model_int_cc <- lm(body_mass_g ~ flipper_length_mm * sex, data = penguins_int)

cat("Additive model (continuous + categorical):\n")
print(summary(model_add_cc))

cat("\nInteraction model (continuous * categorical):\n")
print(summary(model_int_cc))

cat("\nNested model comparison (is flipper_length_mm:sex needed?):\n")
print(anova(model_add_cc, model_int_cc))

# 3. Categorical x Categorical interaction
# Question: Is the male-female body mass gap the same across species?
model_add_cat <- lm(body_mass_g ~ species + sex, data = penguins_int)
model_int_cat <- lm(body_mass_g ~ species * sex, data = penguins_int)

cat("\nAdditive model (species + sex):\n")
print(summary(model_add_cat))

cat("\nInteraction model (species * sex):\n")
print(summary(model_int_cat))

cat("\nNested model comparison (is species:sex needed?):\n")
print(anova(model_add_cat, model_int_cat))

# 4. Predicted means for species x sex interaction model
cell_grid <- expand_grid(
  species = levels(penguins_int$species),
  sex = levels(penguins_int$sex)
)
cell_grid$pred_body_mass_g <- predict(model_int_cat, newdata = cell_grid)

cat("\nPredicted means from species * sex model:\n")
print(cell_grid)

# 5. Visuals
# Plot A: Different slopes by sex (continuous x categorical interaction)
ggplot(penguins_int, aes(x = flipper_length_mm, y = body_mass_g, color = sex)) +
  geom_point(alpha = 0.45) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Interaction Check: flipper_length_mm * sex",
    subtitle = "If lines are non-parallel, interaction may be present",
    x = "Flipper Length (mm)",
    y = "Body Mass (g)"
  ) +
  theme_minimal(base_size = 12)

# Plot B: Interaction profile plot (species x sex)
ggplot(cell_grid, aes(x = species, y = pred_body_mass_g, group = sex, color = sex)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  labs(
    title = "Interaction Profile: species * sex",
    subtitle = "Non-parallel profiles indicate interaction effects",
    x = "Species",
    y = "Predicted Body Mass (g)"
  ) +
  theme_minimal(base_size = 12)
