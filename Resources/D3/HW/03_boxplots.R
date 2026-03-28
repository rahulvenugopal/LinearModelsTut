# ============================================================
#  Script 03 — Box Plots & Dot Plots
#  Dataset : palmerpenguins::penguins
#  Packages: tidyverse, palmerpenguins
# ============================================================
#
#  By the end of this script you will be able to:
#    • Read and interpret a box plot
#    • Build a box plot with geom_boxplot()
#    • Overlay raw data with geom_jitter() and geom_point()
#    • Use position_jitterdodge() to align jittered points with dodged boxes
#    • Understand when box plots alone can be misleading
#    • Add summary statistics (medians, means) as extra layers
#
# ============================================================

library(tidyverse)
library(palmerpenguins)

penguins_clean <- penguins |> drop_na()


# ── 0. Reading a box plot — quick anatomy ─────────────────────────────────────
#
#   ┌────────────┐  ← Q3 (75th percentile)     ─── whisker: Q3 + 1.5 × IQR
#   │            │                                  (or max value if smaller)
#   │────────────│  ← median (Q2, 50th perc.)
#   │            │
#   └────────────┘  ← Q1 (25th percentile)     ─── whisker: Q1 − 1.5 × IQR
#                                                   (or min value if larger)
#       ●           ← outlier (beyond whiskers)
#
#  IQR = Inter-Quartile Range = Q3 − Q1  (height of the box)


# ── 1. Your first box plot ────────────────────────────────────────────────────

ggplot(penguins_clean,
       aes(x = species, y = bill_length_mm)) +
  geom_boxplot()

# Compare distributions across species — already informative!

# Exercise 1 ✏️
# Switch the x and y: put species on the y axis.
# Which orientation do you prefer for reading labels?


# ── 2. Adding colour with fill ────────────────────────────────────────────────

ggplot(penguins_clean,
       aes(x = species, y = bill_length_mm, fill = species)) +
  geom_boxplot()

# Colour by sex instead — grouped box plot
ggplot(penguins_clean,
       aes(x = species, y = bill_length_mm, fill = sex)) +
  geom_boxplot()

# 🔑 When fill is a different column from x, ggplot2 automatically
#    DODGES (side-by-side) the boxes within each x group.


# ── 3. The problem with box plots alone ───────────────────────────────────────

# Box plots hide the raw data.  Two very different distributions
# can produce identical boxes.  The fix: show the actual points.

# geom_jitter() = geom_point() + random horizontal noise
ggplot(penguins_clean,
       aes(x = species, y = bill_length_mm)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.4)

# 🔑 Draw order matters: geom_jitter() on TOP of geom_boxplot()
#    means points appear in front of the boxes.

# Exercise 2 ✏️
# Swap the layer order — put geom_jitter() BEFORE geom_boxplot().
# What changes visually and why?


# ── 4. Combining fill groups with jittered points ────────────────────────────

# When we have a fill = sex grouping, jitter alone mis-aligns points.
# Use position_jitterdodge() to jitter AND dodge simultaneously.

set.seed(42)   # makes the jitter reproducible

ggplot(penguins_clean,
       aes(x = species, y = bill_length_mm, fill = sex, colour = sex)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +  # hide default outlier dots
  geom_point(
    shape    = 21,
    size     = 2,
    alpha    = 0.6,
    position = position_jitterdodge(jitter.width = 0.15, seed = 42)
  )

# 🔑 outlier.shape = NA removes the default outlier markers from geom_boxplot()
#    so they don't get plotted twice (once by the box, once by geom_point()).


# ── 5. Tuning point appearance ────────────────────────────────────────────────

ggplot(penguins_clean,
       aes(x = species, y = bill_length_mm, fill = sex)) +
  geom_boxplot(
    alpha         = 0.3,
    outlier.shape = NA,
    colour        = "grey40"    # box border colour — fixed, not mapped
  ) +
  geom_point(
    aes(colour = sex),
    shape    = 16,              # solid circle — no border
    size     = 2.5,
    alpha    = 0.5,
    position = position_jitterdodge(jitter.width = 0.15, seed = 42)
  ) +
  labs(
    x     = NULL,
    y     = "Bill length (mm)",
    fill  = "Sex",
    colour = "Sex",
    title = "Bill length by species and sex"
  )

# Exercise 3 ✏️
# Change the shape to 21 (open circle) and set colour = "white" for
# geom_point() so the fill colour shows through.
# Does that look better or worse?  Why might you choose each?


# ── 6. Adding summary statistics as extra layers ──────────────────────────────

# Compute medians to overlay as large dots
species_sex_medians <- penguins_clean |>
  summarise(
    median_bill = median(bill_length_mm),
    .by = c(species, sex)
  )

ggplot(penguins_clean,
       aes(x = species, y = bill_length_mm, fill = sex, colour = sex)) +
  # Raw data — small, transparent
  geom_point(
    shape    = 16,
    size     = 2,
    alpha    = 0.35,
    position = position_jitterdodge(jitter.width = 0.15, seed = 42)
  ) +
  # Medians — large, opaque, from a DIFFERENT data frame
  geom_point(
    data     = species_sex_medians,
    aes(y    = median_bill),
    shape    = 18,    # diamond
    size     = 6,
    alpha    = 1,
    position = position_dodge(width = 0.75)
  ) +
  labs(
    x      = NULL,
    y      = "Bill length (mm)",
    colour = "Sex", fill = "Sex",
    title  = "Median bill length (◆) overlaid on raw observations"
  )

# Exercise 4 ✏️
# Add geom_boxplot() back into this chart (between the two geom_point calls).
# Set outlier.shape = NA and alpha = 0.2 on the box.
# Now you have raw data + distribution shape + summary statistic in one chart!


# ── 7. Violin plot — another distribution alternative ────────────────────────

# A violin is a mirrored density curve — shows the shape of the distribution
ggplot(penguins_clean,
       aes(x = species, y = flipper_length_mm, fill = species)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.8, outlier.shape = NA) +
  labs(
    x = NULL, y = "Flipper length (mm)", fill = "Species",
    title = "Violin + box: shape and summary together"
  ) +
  theme(legend.position = "none")

# 🔑 The narrow "waist" on the Gentoo violin around 195 mm —
#    a box plot alone would completely hide that bimodal hint.


# ── Final challenge 🏆 ────────────────────────────────────────────────────────
#
# Build a chart that answers: "How does body mass vary by island and sex?"
#
# Requirements:
#   1. x = island, y = body_mass_g, fill = sex
#   2. Transparent box plots (alpha = 0.3, no outlier markers)
#   3. Jitter-dodged raw points on top (shape = 21, colour = "white")
#   4. Large median diamonds from a summary data frame
#   5. Proper axis labels (include units in y label)
#   6. A meaningful opinionated title
#
# Bonus: sort the islands so the one with the highest overall median
#        body mass appears last (hint: fct_reorder from Script 01!).

