# Understand random intercept model

# Load libraries
library(lme4)
library(lmerTest)   # adds p-values to summary()
library(palmerpenguins)

# Get some clean data
data <- penguins_clean %>% 
  drop_na()

# Drop a random intercept model
# During demo, show without lmerTest first
model_1 <- lmer(data = data,
                body_mass_g ~ flipper_length_mm + (1|species))
summary(model_1)

# Better understanding the terms
plot(model_1)

library(sjPlot)

# Visualise random effects 
(re.effects <- plot_model(model_1, 
                          type = "re", show.values = TRUE))

# Bring in other models
model_2 <- lm(data = data,
              body_mass_g ~ flipper_length_mm)
summary(model_2)

model_3 <- lm(data = data,
              body_mass_g ~ bill_length_mm)
summary(model_3)

model_4 <- lm(data = data,
              body_mass_g ~ flipper_length_mm + bill_length_mm)
summary(model_4)

model_5 <- lm(data = data,
              body_mass_g ~ flipper_length_mm + bill_length_mm + sex)
summary(model_5)

anova(model_1, model_2, model_3, model_4, model_5)
