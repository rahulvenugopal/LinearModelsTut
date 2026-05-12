# Understand the difference between mediation and Multiple regression

# author @ Rahul Venugopal on 11th May 2026

# Load libraries
library(palmerpenguins)
library(mediation)

# Clean data — drop rows with any NAs in our three variables
penguins_clean <- na.omit(penguins[, c("body_mass_g", "flipper_length_mm", "bill_length_mm")])

# Scale variables so coefficients are comparable across models
penguins_s <- as.data.frame(scale(penguins_clean))

#  APPROACH 1: MULTIPLE REGRESSION
mreg <- lm(body_mass_g ~ bill_length_mm + flipper_length_mm, data = penguins_s)
summary(mreg)

#  APPROACH 2: MEDIATION ANALYSIS

#  Path a:   flipper_length ~ bill_length
#  Path b+c': body_mass    ~ bill_length + flipper_length
#  Path c:   body_mass    ~ bill_length  (total effect, no mediator)
# ============================================================

model_c  <- lm(body_mass_g ~ bill_length_mm, data = penguins_s)
summary(model_c)

# Path a -- X predicts M
model_a  <- lm(flipper_length_mm ~ bill_length_mm, data = penguins_s)
summary(model_a)

# Paths b and c' -- M and X jointly predict Y
cat("--- Paths b and c': body_mass ~ bill_length + flipper_length ---\n\n")
model_bc <- lm(body_mass_g ~ bill_length_mm + flipper_length_mm, data = penguins_s)
summary(model_bc)

# Manual decomposition
a   <- coef(model_a)["bill_length_mm"]
b   <- coef(model_bc)["flipper_length_mm"]
c   <- coef(model_c)["bill_length_mm"]
c_p <- coef(model_bc)["bill_length_mm"]

cat(sprintf("
  path c   (total effect, bill -> body_mass):               %.3f
  path a   (bill_length -> flipper_length):                 %.3f
  path b   (flipper_length -> body_mass | bill_length):     %.3f
  path c'  (direct: bill_length -> body_mass | flipper):    %.3f
  indirect effect a*b (mediated via flipper_length):        %.3f
  proportion mediated (a*b / c):                            %.1f%%\n\n",
            c, a, b, c_p, a*b, 100*(a*b)/c))


# ============================================================
#  FORMAL BOOTSTRAP TEST
# ============================================================

set.seed(42)
med_out <- mediate(
  model.m  = model_a,
  model.y  = model_bc,
  treat    = "bill_length_mm",
  mediator = "flipper_length_mm",
  boot     = TRUE,
  sims     = 1000
)
summary(med_out)

cat("
  ACME  = indirect effect (a*b): how much of bill_length's effect
          on body_mass travels THROUGH flipper_length
  ADE   = direct effect (c'):    bill_length -> body_mass bypassing flipper
  Total = ACME + ADE = path c
\n")
