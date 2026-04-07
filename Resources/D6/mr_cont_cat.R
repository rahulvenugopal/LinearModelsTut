# Day 6 - Multiple Regression Tutorial 1
# Continuous Y with one continuous X and one categorical X
# Model focus: body_mass_g ~ flipper_length_mm + sex

# 1. Load libraries and data
library(tidyverse)
library(palmerpenguins)

penguins_cc <- penguins %>%
  select(body_mass_g, flipper_length_mm, sex) %>%
  drop_na() %>%
  mutate(sex = factor(sex))

cat("Rows used:", nrow(penguins_cc), "\n")
cat("Reference level for sex:", levels(penguins_cc$sex)[1], "\n\n")

# 2. Fit additive multiple regression
# Interpretation:
#   - flipper_length_mm coefficient = expected change in body mass per 1 mm increase
#     while holding sex constant.
#   - sexmale coefficient = expected male-female mean difference
#     at the same flipper length.
model_add <- lm(body_mass_g ~ flipper_length_mm + sex, data = penguins_cc)
summary(model_add)

cat("\n95% confidence intervals for coefficients:\n")
print(confint(model_add))

# 3. Compare with a single-predictor model
model_simple <- lm(body_mass_g ~ flipper_length_mm, data = penguins_cc)

cat("\nNested model comparison (does adding sex help?):\n")
print(anova(model_simple, model_add))

# 4. Optional slope check: do males and females need separate slopes?
model_interaction <- lm(body_mass_g ~ flipper_length_mm * sex, data = penguins_cc)

cat("\nAdditive vs interaction model comparison:\n")
print(anova(model_add, model_interaction))

# 5. Predict adjusted values for selected cases
new_cases <- tibble(
  flipper_length_mm = c(185, 200, 215, 200),
  sex = factor(c("female", "female", "female", "male"), levels = levels(penguins_cc$sex))
)

pred_out <- cbind(
  new_cases,
  predict(model_add, newdata = new_cases, interval = "confidence")
)

cat("\nPredicted body mass from additive model:\n")
print(pred_out)

# 6. Visual: raw data + fitted lines by sex
ggplot(penguins_cc, aes(x = flipper_length_mm, y = body_mass_g, color = sex)) +
  geom_point(alpha = 0.55) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Multiple Regression: Continuous + Categorical Predictor",
    subtitle = "Outcome: body_mass_g | Predictors: flipper_length_mm + sex",
    x = "Flipper Length (mm)",
    y = "Body Mass (g)"
  ) +
  theme_minimal(base_size = 12)
