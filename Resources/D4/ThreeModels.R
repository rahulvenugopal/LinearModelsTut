library(tidyverse)
library(palmerpenguins)
library(easystats)

data <- penguins %>%
  select(body_mass_g, species, sex, flipper_length_mm) %>%
  drop_na()


# Model 1 -----------------------------------------------------------------

# Continuous Y ~ Continuous X
# Question: Can flipper length predict body mass?

model1 <- lm(body_mass_g ~ flipper_length_mm, data = data)
summary(model1)

model_dashboard(model1)

# Reading the summary:
#
#   (Intercept)  ≈ -5781  → predicted body mass when flipper length = 0mm
#                           biologically meaningless, but mathematically required
#
#   flipper_length_mm ≈ 49.7  → for every 1mm longer flipper, body mass
#                                increases by ~50g  ← this is the slope
#
#   Std. Error  → margin of uncertainty around the estimate
#   t value     → Estimate ÷ Std. Error; bigger = more reliable signal
#   Pr(>|t|)    → p-value; tiny here means flipper length is a real predictor
#
#   R-squared ≈ 0.76  → flipper length alone explains 76% of variation in
#                        body mass. Strong single-predictor result.
#
# Intuition: the model is just y = mx + c drawn through the scatterplot

ggplot(data, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  labs(title = "Continuous ~ Continuous",
       x = "Flipper Length (mm)", y = "Body Mass (g)") +
  theme_minimal()


# ── 2. Continuous Y ~ Categorical X ─────────────────────────────────────────
# Question: Do male and female penguins differ in body mass?
# Same lm() as above — R automatically converts "male/female" into 1/0
# This is called dummy coding

model2 <- lm(body_mass_g ~ sex, data = data)
summary(model2)

model_dashboard(model2)

# Reading the summary:
#
#   (Intercept) ≈ 3862g  → average body mass of the REFERENCE group (female)
#                           R picks alphabetical order, so female = reference
#
#   sexmale ≈ +683g  → males weigh ~683g MORE than females on average
#                       this is not a slope anymore — it's a group difference
#
#   Pr(>|t|) tiny  → this difference is statistically significant
#
#   R-squared ≈ 0.18  → sex alone explains only 18% of body mass variation
#                        much weaker than flipper length (76%) — sex isn't
#                        the whole story
#
# Intuition: the model is estimating two group means and asking
#            "is the gap between them real or just noise?"

ggplot(data, aes(x = sex, y = body_mass_g)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1.2) +
  labs(title = "Continuous ~ Categorical",
       x = "Sex", y = "Body Mass (g)") +
  theme_minimal() +
  theme(legend.position = "none")


# ── 3. Categorical Y ~ Continuous X ─────────────────────────────────────────
# Question: Does body mass predict whether a penguin is male or female?
# Y is now binary (male/female) so lm() breaks down — predicted values
# could go below 0 or above 1, which makes no sense as probabilities.
# glm(family = binomial) solves this by modelling LOG-ODDS instead of
# raw probabilities, then squashing predictions into the [0, 1] range
# via the logistic (sigmoid) curve. This is logistic regression.

model3 <- glm(sex ~ body_mass_g, data = data, family = binomial)
summary(model3)

# Reading the summary:
#
#   (Intercept) ≈ -5.16  → log-odds of being male when body mass = 0g
#                           not directly interpretable, same role as before
#
#   body_mass_g ≈ 0.0013  → for every 1g increase in body mass, the
#                            log-odds of being male increases by 0.0013
#                            hard to feel, so convert to odds ratios:
#
#       exp(coef(model3)["body_mass_g"] * 100)  # per 100g
#       ≈ 1.13  →  every 100g heavier = 13% higher odds of being male
#
#   z value  → same idea as t value: Estimate ÷ Std. Error
#   Pr(>|z|) tiny  → body mass is a genuine predictor of sex
#
#   Null deviance: 460  → how badly a model with NO predictors fits
#   Residual deviance: 354  → how badly THIS model fits
#   Drop of ~106 points  → what body mass bought us; larger drop = better
#                           predictor (this is the logistic equivalent of R²)
#
# Intuition: instead of a straight line, the model draws an S-shaped curve
#            that maps body mass onto a probability between 0 and 1

exp(coef(model3)["body_mass_g"] * 100)   # odds ratio per 100g

pred_df <- tibble(
  body_mass_g = seq(min(data$body_mass_g), max(data$body_mass_g), length.out = 200)
)
pred_df$prob_male <- predict(model3, newdata = pred_df, type = "response")

ggplot() +
  geom_jitter(data = data,
              aes(x = body_mass_g, y = as.numeric(sex == "male")),
              height = 0.05, alpha = 0.3) +
  geom_line(data = pred_df, aes(x = body_mass_g, y = prob_male),
            color = "tomato", linewidth = 1) +
  labs(title = "Categorical Y ~ Continuous X (Logistic)",
       x = "Body Mass (g)", y = "P(male)") +
  theme_minimal()


# ── Comparing all three models ───────────────────────────────────────────────
# Model 1 R²  = 0.76  — flipper length is a strong predictor of body mass
# Model 2 R²  = 0.18  — sex explains some body mass variation, but not much
# Model 3 deviance drop = 106  — body mass meaningfully predicts sex