# Day 5 – Correlation Revealed: The Pearson-Spearman-Regression Connection
# Goal: Understand how r and beta relate, and why scaling doesn't break the model.

# 1. Load libraries and data
library(palmerpenguins)
library(ggplot2)

# Use Adelie penguins for a cleaner single-group demonstration
penguins_clean <- na.omit(penguins[penguins$species == "Adelie", ])

# Variables of interest: x = flipper_length_mm, y = body_mass_g
x <- penguins_clean$flipper_length_mm
y <- penguins_clean$body_mass_g

# 2. Pearson vs. Spearman
r_pearson <- cor(x, y, method = "pearson")
r_spearman <- cor(x, y, method = "spearman")

# 3. The Beta-Correlation Bridge
# Formula: beta_1 = r * (sd_y / sd_x)
model <- lm(y ~ x)
beta_1 <- coef(model)["x"]

# Verification
sd_x <- sd(x)
sd_y <- sd(y)
calculated_beta <- r_pearson * (sd_y / sd_x)

cat("Regression Slope (beta_1): ", beta_1, "\n")
cat("Calculated Slope (r * sd_y/sd_x): ", calculated_beta, "\n")
cat("Difference: ", beta_1 - calculated_beta, " (Should be near zero)\n\n")

# 4. Invariance to Scaling and Shifting
# Let's transform X: scale it by 10 and shift it by 100
x_scaled <- x * 10
x_shifted <- x + 100

# Correlation check
r_original <- cor(x, y)
r_scaled   <- cor(x_scaled, y)
r_shifted  <- cor(x_shifted, y)

cat("Correlation (Original): ", r_original, "\n")
cat("Correlation (X * 10):   ", r_scaled, "\n")
cat("Correlation (X + 100):  ", r_shifted, "\n")
cat("=> r is invariant to linear transformations!\n\n")

# Regression check
beta_scaled  <- coef(lm(y ~ x_scaled))["x_scaled"]
beta_shifted <- coef(lm(y ~ x_shifted))["x_shifted"]

cat("Beta (Original): ", beta_1, "\n")
cat("Beta (X * 10):   ", beta_scaled, " (Original / 10 = ", beta_1/10, ")\n")
cat("Beta (X + 100):  ", beta_shifted, " (No change from original)\n")
cat("=> Beta scales with X but is unaffected by horizontal shifts!\n")

# 5. Visualizing the relationships
ggplot(penguins_clean, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(alpha = 0.6, color = "#818CF8") +
  geom_smooth(method = "lm", se = TRUE, color = "white") +
  theme_minimal() +
  labs(title = "Pearson r and Regression Slope",
       subtitle = paste("r =", round(r_pearson, 3), "| beta =", round(beta_1, 2)),
       x = "Flipper Length (mm)",
       y = "Body Mass (g)") +
  theme(plot.background = element_rect(fill = "#050814", color = NA),
        panel.grid = element_line(color = "#1E293B"),
        text = element_text(color = "#E2E8F0"))
