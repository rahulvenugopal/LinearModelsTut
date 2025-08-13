# Understand linear mixed effects  model

# author @ Rahul Venugopal on 20th July 2025

# Load libraries
library(dplyr)
library(lme4)

# Load tutorial data from Bodo winter
politeness = read.csv('data/politeness_data.csv')

boxplot(frequency ~ attitude + gender,
        col = c("white", "lightgray"), politeness)

lmer(frequency ~ attitude + gender, data = politeness)
# Error in mkReTrms(findbars(RHSForm(formula)), fr) : No random effects
# terms specified in formula

politeness.model = lmer(
  frequency ~ attitude + gender + (1|subject) + (1|scenario),
  data = politeness
)

summary(politeness.model)

politeness.null = lmer(
  frequency ~ gender + (1|subject) + (1|scenario),
  data = politeness, REML = FALSE
)

# * Include attitude into the model

politeness.model = lmer(
  frequency ~ attitude + gender + (1|subject) + (1|scenario),
  data = politeness, REML = FALSE
)

anova(politeness.null, politeness.model)
