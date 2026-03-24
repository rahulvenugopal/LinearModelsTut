# ==============================================================================
# Day 2 : Data Wrangling with dplyr & tidyr
# Dataset : Palmer Penguins
# ==============================================================================
# By the end of this script you will be able to:
#   1. Use all core dplyr verbs (filter, select, mutate, summarise, arrange)
#   2. Group data with group_by()
#   3. Chain operations with the pipe |>
#   4. Reshape data: wide → long (pivot_longer / gather)
#   5. Reshape data: long → wide (pivot_wider / spread)
# ==============================================================================

# ---- 0. Setup ----------------------------------------------------------------
# install.packages("palmerpenguins")        # run once if not installed
# install.packages("tidyverse")             # run once if not installed

library(palmerpenguins)
library(tidyverse)

# Chaining
penguins |>
  filter(species == "Adelie",
         island == "Dream",
         body_mass_g > 4000)

# OR logic — use |
# Penguins that are either Chinstrap OR Gentoo
penguins |>
  filter(species == "Chinstrap" | species == "Gentoo")

# Shortcut for multiple OR on the same column — %in%
penguins |>
  filter(species %in% c("Chinstrap", "Gentoo"))

# 🧪Filter penguins whose flipper length is between 190 and 210 mm

# Select specific columns
penguins |>
  select(species, island, body_mass_g)

# Drop a column with minus sign
penguins |>
  select(-year)

# Helper functions: starts_with, ends_with, contains
penguins |>
  select(species, starts_with("bill"))

# 🧪 TRY IT: Select only columns that end with "_mm"

# Create multiple columns at once
penguins |>
  mutate(
    bill_ratio    = bill_length_mm / bill_depth_mm,
    flipper_cm    = flipper_length_mm / 10,
    mass_category = if_else(body_mass_g > 4000, "heavy", "light")
  ) |>
  select(species, bill_ratio, flipper_cm, mass_category)

# 🧪 TRY IT: Create a column called bill_area = bill_length_mm * bill_depth_mm
