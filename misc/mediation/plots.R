# Get a grip on mediation models

# Load libraries
library(palmerpenguins)
library(tidyverse)
library(broom)
library(patchwork)

# Clean and scale
penguins_clean <- na.omit(penguins[, c("bill_length_mm", "flipper_length_mm", "body_mass_g")])
penguins_s     <- as.data.frame(scale(penguins_clean))

# Set the theme
theme_blog <- theme_minimal(base_size = 16) +
  theme(
    plot.title       = element_text(face = "bold", size = 18, margin = margin(b = 6)),
    plot.subtitle    = element_text(color = "grey40", size = 11, margin = margin(b = 10)),
    plot.caption     = element_text(color = "grey50", size = 9, margin = margin(t = 10)),
    axis.title       = element_text(size = 16),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "white", color = NA)
  )

# Scatter plot and associations

scatter_bm_bl <- ggplot(penguins_s, aes(x = bill_length_mm, y = body_mass_g)) +
  geom_point(alpha = 0.4, color = "#2171b5", size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#08519c", linewidth = 0.9) +
  labs(x = "Bill length (SD)", y = "Body mass (SD)",
       title = "Bill length vs Body mass",
       subtitle = paste0("r = ", round(cor(penguins_s$bill_length_mm, penguins_s$body_mass_g), 2))) +
  theme_blog

scatter_fl_bm <- ggplot(penguins_s, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(alpha = 0.4, color = "#238b45", size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#006d2c", linewidth = 0.9) +
  labs(x = "Flipper length (SD)", y = "Body mass (SD)",
       title = "Flipper length vs Body mass",
       subtitle = paste0("r = ", round(cor(penguins_s$flipper_length_mm, penguins_s$body_mass_g), 2))) +
  theme_blog

scatter_bl_fl <- ggplot(penguins_s, aes(x = bill_length_mm, y = flipper_length_mm)) +
  geom_point(alpha = 0.4, color = "#d94801", size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#a63603", linewidth = 0.9) +
  labs(x = "Bill length (SD)", y = "Flipper length (SD)",
       title = "Bill length vs Flipper length",
       subtitle = paste0("r = ", round(cor(penguins_s$bill_length_mm, penguins_s$flipper_length_mm), 2))) +
  theme_blog

fig1 <- scatter_bm_bl + scatter_fl_bm + scatter_bl_fl +
  plot_annotation(
    title   = "All three variables are correlated",
    subtitle = "The puzzle: If bill and flipper are both correlated with body mass, what happens when we put them together?",
    caption  = "Data: palmerpenguins | Standardised variables",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(color = "grey40", size = 12),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

ggsave("Associations.png", fig1,
       width = 12, height = 10, dpi = 300)

# Regression coefficients

model_simple   <- lm(body_mass_g ~ bill_length_mm, data = penguins_s)
model_multiple <- lm(body_mass_g ~ bill_length_mm + flipper_length_mm, data = penguins_s)

coef_df <- bind_rows(
  tidy(model_simple,   conf.int = TRUE) %>% mutate(model = "Simple: body_mass ~ bill_length"),
  tidy(model_multiple, conf.int = TRUE) %>% mutate(model = "Multiple: body_mass ~ bill_length + flipper_length")
) %>%
  filter(term == "bill_length_mm") %>%
  mutate(
    significant = p.value < 0.05,
    model = factor(model, levels = c(
      "Simple: body_mass ~ bill_length",
      "Multiple: body_mass ~ bill_length + flipper_length"
    )),
    label = paste0("β = ", round(estimate, 3),
                   "\np ", ifelse(significant, "< 0.05", "= 0.332"))
  )

fig2 <- ggplot(coef_df, aes(x = model, y = estimate, color = significant)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.7) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.12, linewidth = 0.9) +
  geom_point(size = 5) +
  geom_text(aes(label = label), vjust = -2.25, size = 3.8,
            fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c("TRUE" = "#08519c", "FALSE" = "#cb181d"),
                     labels = c("TRUE" = "Significant", "FALSE" = "Not significant"),
                     name   = NULL) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_y_continuous(limits = c(-0.15, 0.85)) +
  labs(
    x = NULL, y = "Coefficient for bill_length_mm (95% CI)",
    title   = "Figure 2 — The coefficient collapse",
    subtitle = "Bill length's effect on body mass drops from 0.595 to 0.041 once flipper length enters the model",
    caption  = "Data: palmerpenguins | Standardised variables"
  ) +
  theme_blog +
  theme(legend.position = "bottom")

ggsave("coefficient_collapse.png", fig2,
       width = 10, height = 10, dpi = 300)

# ============================================================
#  FIGURE 3 — Path diagram with actual coefficients
# ============================================================

model_a  <- lm(flipper_length_mm ~ bill_length_mm, data = penguins_s)
model_bc <- lm(body_mass_g ~ bill_length_mm + flipper_length_mm, data = penguins_s)
model_c  <- lm(body_mass_g ~ bill_length_mm, data = penguins_s)

a   <- round(coef(model_a)["bill_length_mm"], 3)

# In the Baron & Kenny framework, path b is defined as the effect of 
# M on Y controlling for X. Not the simple effect of M on Y alone.

# If you used lm(body_mass_g ~ flipper_length_mm) in isolation, that coefficient
# would include all the ways flipper length relates to body mass — including the
# part that flows back through bill length. You'd be double-counting.
b   <- round(coef(model_bc)["flipper_length_mm"], 3)
c_p <- round(coef(model_bc)["bill_length_mm"], 3)
c   <- round(coef(model_c)["bill_length_mm"], 3)
ab  <- round(a * b, 3)

# Build diagram using annotate() on a blank canvas
fig3 <- ggplot() +
  xlim(0, 10) + ylim(0, 6) +
  coord_fixed() +
  
  # Node boxes
  annotate("rect", xmin = 0.3, xmax = 2.7, ymin = 2.8, ymax = 4.2,
           fill = "#deebf7", color = "#08519c", linewidth = 0.8, radius = unit(4, "pt")) +
  annotate("rect", xmin = 3.9, xmax = 6.5, ymin = 4.2, ymax = 5.7,
           fill = "#feedde", color = "#d94801", linewidth = 0.8, radius = unit(4, "pt")) +
  annotate("rect", xmin = 7.3, xmax = 9.7, ymin = 2.8, ymax = 4.2,
           fill = "#e5f5e0", color = "#238b45", linewidth = 0.8, radius = unit(4, "pt")) +
  
  # Node labels
  annotate("text", x = 1.5, y = 3.7, label = "Bill length", fontface = "bold", size = 4.2) +
  annotate("text", x = 1.5, y = 3.2, label = "X", color = "grey40", size = 3.8) +
  annotate("text", x = 5.2, y = 5.15, label = "Flipper length", fontface = "bold", size = 4.2) +
  annotate("text", x = 5.2, y = 4.62, label = "M (mediator)", color = "grey40", size = 3.8) +
  annotate("text", x = 8.5, y = 3.7, label = "Body mass", fontface = "bold", size = 4.2) +
  annotate("text", x = 8.5, y = 3.2, label = "Y", color = "grey40", size = 3.8) +
  
  # Arrow a: X -> M
  annotate("segment", x = 2.7, xend = 3.85, y = 3.9, yend = 4.6,
           arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
           color = "#d94801", linewidth = 1.1) +
  annotate("text", x = 3.1, y = 4.45, label = paste0("a = ", a),
           color = "#d94801", fontface = "bold", size = 4, hjust = 1) +
  
  # Arrow b: M -> Y
  annotate("segment", x = 6.55, xend = 7.25, y = 4.6, yend = 3.9,
           arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
           color = "#d94801", linewidth = 1.1) +
  annotate("text", x = 7.1, y = 4.45, label = paste0("b = ", b),
           color = "#d94801", fontface = "bold", size = 4, hjust = 0) +
  
  # Arrow c': X -> Y (direct, curved below)
  annotate("segment", x = 2.75, xend = 7.25, y = 3.1, yend = 3.1,
           arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
           color = "#238b45", linewidth = 1.1) +
  annotate("text", x = 5.0, y = 2.75, label = paste0("c' = ", c_p, "  (direct effect, n.s.)"),
           color = "#238b45", fontface = "bold", size = 3.8) +
  
  # Indirect effect label
  annotate("text", x = 5.0, y = 1.9,
           label = paste0("Indirect effect (a \u00d7 b) = ", a, " \u00d7 ", b, " = ", ab),
           color = "#d94801", fontface = "bold", size = 4.2) +
  annotate("text", x = 5.0, y = 1.4,
           label = paste0("Total effect (c) = ", c, "   |   Proportion mediated = 93.1%"),
           color = "grey30", size = 3.8) +
  
  labs(
    title   = "Mediation path diagram with coefficients",
    subtitle = "93% of bill length's effect on body mass is carried through flipper length",
    caption  = "Data: palmerpenguins | Standardised variables"
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 15, margin = margin(b = 6)),
    plot.subtitle   = element_text(color = "grey40", size = 11, margin = margin(b = 4)),
    plot.caption    = element_text(color = "grey50", size = 9, margin = margin(t = 8)),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin     = margin(15, 15, 15, 15)
  )

ggsave("path_diagram.png", fig3, 
       width = 10, height = 6, dpi = 300)

#  Partial regression plots

# Residualise bill_length and body_mass after removing flipper_length's effect
resid_bill_on_flipper <- residuals(lm(bill_length_mm  ~ flipper_length_mm, data = penguins_s))
resid_body_on_flipper <- residuals(lm(body_mass_g     ~ flipper_length_mm, data = penguins_s))

# Residualise flipper_length and body_mass after removing bill_length's effect
resid_flip_on_bill    <- residuals(lm(flipper_length_mm ~ bill_length_mm, data = penguins_s))
resid_body_on_bill    <- residuals(lm(body_mass_g       ~ bill_length_mm, data = penguins_s))

partial_df <- tibble(
  resid_bill  = resid_bill_on_flipper,
  resid_flip  = resid_flip_on_bill,
  resid_bm_1  = resid_body_on_flipper,
  resid_bm_2  = resid_body_on_bill
)

# Partial slope labels
slope_bill <- round(coef(lm(resid_bm_1 ~ resid_bill, data = partial_df))["resid_bill"], 3)
slope_flip <- round(coef(lm(resid_bm_2 ~ resid_flip, data = partial_df))["resid_flip"], 3)

p4a <- ggplot(partial_df, aes(x = resid_bill, y = resid_bm_1)) +
  geom_point(alpha = 0.4, color = "#cb181d", size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#99000d", linewidth = 0.9) +
  annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,
           label = paste0("Partial slope = ", slope_bill, " (n.s.)"),
           color = "#99000d", fontface = "bold", size = 4) +
  labs(
    x = "Bill length | Flipper length (residuals)",
    y = "Body mass | Flipper length (residuals)",
    title = "Bill length → Body mass",
    subtitle = "After removing flipper length from both variables"
  ) +
  theme_blog

p4b <- ggplot(partial_df, aes(x = resid_flip, y = resid_bm_2)) +
  geom_point(alpha = 0.4, color = "#238b45", size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#006d2c", linewidth = 0.9) +
  annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,
           label = paste0("Partial slope = ", slope_flip, " ***"),
           color = "#006d2c", fontface = "bold", size = 4) +
  labs(
    x = "Flipper length | Bill length (residuals)",
    y = "Body mass | Bill length (residuals)",
    title = "Flipper length → Body mass",
    subtitle = "After removing bill length from both variables"
  ) +
  theme_blog

fig4 <- p4a + p4b +
  plot_annotation(
    title    = "Partial regression plots",
    subtitle = "Left: bill length has no unique effect on body mass once flipper is accounted for\nRight: flipper length retains a strong unique effect on body mass",
    caption  = "Data: palmerpenguins | Standardised variables",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(color = "grey40", size = 11),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

ggsave("partial_regression.png", fig4, 
       width = 11, height = 5, dpi = 300)