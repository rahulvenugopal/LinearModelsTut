# ============================================================
#  Script 02 — The ggplot2 Ecosystem & Scatter Plots
#  Dataset : palmerpenguins::penguins
#  Packages: tidyverse, palmerpenguins
# ============================================================
#
#  By the end of this script you will be able to:
#    • Understand the Grammar of Graphics (gg) mental model
#    • Build a scatter plot layer by layer
#    • Map data variables to visual aesthetics (colour, size, shape)
#    • Add a smoothing line
#    • Use facets to split a chart by a category
#    • Save a plot to disk with ggsave()
#
# ============================================================

library(tidyverse)
library(palmerpenguins)

penguins_clean <- penguins |> drop_na()


# ── 0. The Grammar of Graphics — the mental model ────────────────────────────
#
#  Every ggplot2 chart is built from the same recipe:
#
#   ggplot(data, aes(...))   ← canvas + aesthetic mappings
#     + geom_*()             ← geometric shapes (points, lines, bars …)
#     + scale_*()            ← control axes, colours, sizes
#     + facet_*()            ← small multiples
#     + labs()               ← titles, labels, captions
#     + theme_*() / theme()  ← visual styling
#
#  Each "+" adds a new LAYER.  Layers are drawn in order, so later
#  layers appear on top of earlier ones.


# ── 1. The bare canvas ────────────────────────────────────────────────────────

# Just the coordinate system — no data yet
ggplot(data = penguins_clean)

# Add axes by telling ggplot WHICH columns map to x and y
ggplot(data = penguins_clean,
       mapping = aes(x = flipper_length_mm, y = body_mass_g))

# Still empty! We need a geom to actually draw something.


# ── 2. geom_point() — our first scatter plot ─────────────────────────────────

ggplot(data = penguins_clean,
       mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

# 🔑 Notice: ggplot() uses the + operator (not |>) to add layers.
#    aes() = "aesthetic": a *mapping* from a data column to a visual property.


# ── 3. Aesthetic mappings vs. fixed aesthetics ───────────────────────────────

# Map COLOUR to the species column — different colour per species
ggplot(penguins_clean,
       aes(x = flipper_length_mm, y = body_mass_g, colour = species)) +
  geom_point()

# Map SIZE to body_mass_g as well (a bit redundant, but illustrative)
ggplot(penguins_clean,
       aes(x = flipper_length_mm, y = body_mass_g,
           colour = species, size = body_mass_g)) +
  geom_point()

# Fixed aesthetic — every point gets the same value, set OUTSIDE aes()
ggplot(penguins_clean,
       aes(x = flipper_length_mm, y = body_mass_g, colour = species)) +
  geom_point(size = 3, alpha = 0.7)   # size and alpha are FIXED here

# 🔑 Rule of thumb:
#    Inside  aes()  → mapped to a data column  (varies point-to-point)
#    Outside aes()  → fixed value              (same for every point)

# Exercise 1 ✏️
# Add a SHAPE mapping to sex. What does the plot tell you?


# ── 4. Mapping multiple aesthetics — telling a richer story ──────────────────

ggplot(penguins_clean,
       aes(x     = flipper_length_mm,
           y     = body_mass_g,
           colour = species,
           shape  = sex)) +
  geom_point(size = 3, alpha = 0.7)

# Notice how the legend updates automatically!


# ── 5. Adding a smoothing line with geom_smooth() ────────────────────────────

# A single trend line across all data
ggplot(penguins_clean,
       aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE)   # lm = linear model; se = conf. band

# One trend line PER species (colour mapping is inherited by geom_smooth)
ggplot(penguins_clean,
       aes(x      = flipper_length_mm,
           y      = body_mass_g,
           colour = species)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE)  # se = FALSE removes the grey ribbon

# 🔑 Aesthetics set in ggplot() are GLOBAL — every geom inherits them.
#    Aesthetics set inside a specific geom_*() are LOCAL to that layer only.

# Exercise 2 ✏️
# Move the colour = species mapping from ggplot() into geom_point() only.
# What happens to the smooth line?


# ── 6. Labels — always label your axes! ──────────────────────────────────────

ggplot(penguins_clean,
       aes(x      = flipper_length_mm,
           y      = body_mass_g,
           colour = species)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title   = "Larger flippers go hand-in-hand with greater body mass",
    subtitle = "Each point is one penguin; lines show linear trends per species",
    x       = "Flipper length (mm)",
    y       = "Body mass (g)",
    colour  = "Species",
    caption = "Data: {palmerpenguins}"
  )

# Exercise 3 ✏️
# Add a caption and change the y-axis label to include the unit "(g)".


# ── 7. Scales — controlling axes and colour palettes ─────────────────────────

# Change the colour palette to a colourblind-friendly one
ggplot(penguins_clean,
       aes(x = flipper_length_mm, y = body_mass_g, colour = species)) +
  geom_point(size = 3, alpha = 0.6) +
  scale_colour_brewer(palette = "Dark2") +    # RColorBrewer palette
  labs(x = "Flipper length (mm)", y = "Body mass (g)", colour = "Species")

# Format the y-axis with comma separators (nice for large numbers)
ggplot(penguins_clean,
       aes(x = flipper_length_mm, y = body_mass_g, colour = species)) +
  geom_point(size = 3, alpha = 0.6) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x = "Flipper length (mm)", y = "Body mass (g)", colour = "Species")


# ── 8. Facets — small multiples ───────────────────────────────────────────────

# Split into one panel per species (one variable → facet_wrap)
ggplot(penguins_clean,
       aes(x = flipper_length_mm, y = body_mass_g, colour = sex)) +
  geom_point(size = 2, alpha = 0.7) +
  facet_wrap(~ species) +
  labs(x = "Flipper length (mm)", y = "Body mass (g)", colour = "Sex")

# Grid: rows = sex, columns = species (two variables → facet_grid)
ggplot(penguins_clean,
       aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(size = 2, alpha = 0.6) +
  facet_grid(sex ~ species) +
  labs(x = "Flipper length (mm)", y = "Body mass (g)")

# Exercise 4 ✏️
# Create a faceted scatter plot of bill_length_mm vs bill_depth_mm,
# one panel per island, coloured by species.
# What does this reveal about which islands each species lives on?


# ── 9. Saving your plot ───────────────────────────────────────────────────────

# Assign the plot to a variable first
p <- ggplot(penguins_clean,
            aes(x = flipper_length_mm, y = body_mass_g, colour = species)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_colour_brewer(palette = "Dark2") +
  labs(
    title   = "Larger flippers go hand-in-hand with greater body mass",
    x       = "Flipper length (mm)",
    y       = "Body mass (g)",
    colour  = "Species",
    caption = "Data: {palmerpenguins}"
  )

# Save it — ggsave() automatically detects file type from the extension
ggsave("02_scatter_plot.png",
       plot   = p,
       width  = 8,
       height = 5,
       dpi    = 300,
       bg     = "white")


# ── Final challenge 🏆 ────────────────────────────────────────────────────────
#
# Build a publication-ready scatter plot that shows:
#   • bill_length_mm on x, bill_depth_mm on y
#   • colour mapped to species
#   • shape mapped to sex
#   • a linear smooth per species (no standard-error ribbon)
#   • faceted by island
#   • proper axis labels with units
#   • a meaningful title and subtitle
#   • saved to disk at 300 dpi
#
# Hint: you'll notice something surprising about Gentoo penguins.
#       Can you write it as your subtitle?

