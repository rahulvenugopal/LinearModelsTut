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

# Quick look at the data
glimpse(penguins)
head(penguins)

# How many rows and columns?
dim(penguins)

# Any missing values?
colSums(is.na(penguins))

# ==============================================================================
# PART 1 — dplyr VERBS
# ==============================================================================

# ---- 1.1  filter() — pick ROWS by condition ---------------------------------
# Keep only Adelie penguins
adelie <- penguins |>
  filter(species == "Adelie")

adelie

# Multiple conditions (AND logic — comma or &)
# Adelie penguins on Dream island with body mass > 4000 g
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

# 🧪 TRY IT: Filter penguins whose flipper length is between 190 and 210 mm


# ---- 1.2  select() — pick COLUMNS by name -----------------------------------
# Keep only species, island, and body_mass_g
penguins |>
  select(species, island, body_mass_g)

# Drop a column with minus sign
penguins |>
  select(-year)

# Select a range of columns
penguins |>
  select(species:flipper_length_mm)

# Helper functions: starts_with, ends_with, contains
penguins |>
  select(species, starts_with("bill"))

# 🧪 TRY IT: Select only columns that end with "_mm"


# ---- 1.3  mutate() — create or modify columns -------------------------------
# Create a new column: body mass in kilograms
penguins |>
  mutate(body_mass_kg = body_mass_g / 1000) |>
  select(species, body_mass_g, body_mass_kg)

# Create multiple columns at once
penguins |>
  mutate(
    bill_ratio    = bill_length_mm / bill_depth_mm,
    flipper_cm    = flipper_length_mm / 10,
    mass_category = if_else(body_mass_g > 4000, "heavy", "light")
  ) |>
  select(species, bill_ratio, flipper_cm, mass_category)

# 🧪 TRY IT: Create a column called bill_area = bill_length_mm * bill_depth_mm


# ---- 1.4  arrange() — sort rows ---------------------------------------------
# Sort by body mass (ascending by default)
penguins |>
  arrange(body_mass_g)

# Descending order
penguins |>
  arrange(desc(body_mass_g))

# Sort by multiple columns: species first, then body mass descending
penguins |>
  arrange(species, desc(body_mass_g))

# 🧪 TRY IT: Arrange by flipper_length_mm in descending order


# ---- 1.5  summarise() — collapse rows to a summary --------------------------
# Overall mean body mass (na.rm = TRUE to handle missing values)
penguins |>
  summarise(
    mean_mass = mean(body_mass_g, na.rm = TRUE),
    sd_mass   = sd(body_mass_g, na.rm = TRUE),
    n         = n()
  )

# ---- 1.6  group_by() + summarise() — summaries per group --------------------
# Mean body mass per species
penguins |>
  group_by(species) |>
  summarise(
    mean_mass = mean(body_mass_g, na.rm = TRUE),
    sd_mass   = sd(body_mass_g, na.rm = TRUE),
    n         = n()
  )

# Group by TWO variables
penguins |>
  group_by(species, sex) |>
  summarise(
    mean_flipper = mean(flipper_length_mm, na.rm = TRUE),
    .groups = "drop"                  # ungroup automatically after summarise
  )

# 🧪 TRY IT: Find the mean bill_length_mm per island


# ---- 1.7  count() — quick frequency table ------------------------------------
penguins |>
  count(species)

penguins |>
  count(species, island, sort = TRUE)


# ---- 1.8  Putting it all together — a mini pipeline -------------------------
# Question: For each species, what is the mean bill ratio for heavy penguins?

penguins |>
  drop_na() |>                                       # remove rows with NAs
  filter(body_mass_g > 4000) |>                      # heavy penguins only
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) |>
  group_by(species) |>
  summarise(
    mean_ratio = mean(bill_ratio),
    n          = n()
  ) |>
  arrange(desc(mean_ratio))


# ==============================================================================
# PART 2 — RESHAPING DATA (tidyr)
# ==============================================================================
# Many statistical functions and ggplot2 expect data in "long" format.
# Let's learn to move between wide and long formats.
# ------------------------------------------------------------------------------

# ---- 2.0  Create a small wide dataset to work with --------------------------
penguin_summary <- penguins |>
  drop_na() |>
  group_by(species) |>
  summarise(
    mean_bill_length = mean(bill_length_mm),
    mean_bill_depth  = mean(bill_depth_mm),
    mean_flipper     = mean(flipper_length_mm)
  )

penguin_summary
# This is WIDE: each measurement is its own column


# ---- 2.1  pivot_longer() — wide → long (modern way) -------------------------
# Convert the three measurement columns into two columns:
#   "measurement" (name) and "value" (number)

penguin_long <- penguin_summary |>
  pivot_longer(
    cols      = starts_with("mean_"),    # which columns to pivot
    names_to  = "measurement",           # new column for the old column names
    values_to = "value"                  # new column for the values
  )

penguin_long
# Now each row is one species × one measurement — this is LONG format


# ---- 2.2  pivot_wider() — long → wide (modern way) --------------------------
# Reverse it: go from long back to wide

penguin_wide <- penguin_long |>
  pivot_wider(
    names_from  = measurement,           # column whose values become new column names
    values_from = value                  # column whose values fill the new columns
  )

penguin_wide
# Back to the original wide format!


# ---- 2.3  gather() — wide → long (legacy tidyr, pre-2019) -------------------
# gather() does the same as pivot_longer() but with older syntax
# You may see this in older code / tutorials

penguin_long_legacy <- penguin_summary |>
  gather(
    key   = "measurement",               # new column for names (like names_to)
    value = "value",                      # new column for values (like values_to)
    -species                              # columns NOT to gather (keep species as-is)
  )

penguin_long_legacy
# Identical result to pivot_longer()


# ---- 2.4  spread() — long → wide (legacy tidyr, pre-2019) -------------------
# spread() does the same as pivot_wider()

penguin_wide_legacy <- penguin_long_legacy |>
  spread(
    key   = measurement,                 # column to spread into new columns
    value = value                        # column with the values
  )

penguin_wide_legacy
# Identical result to pivot_wider()


# ---- 2.5  A real-world example: making ggplot-ready data --------------------
# Suppose we want to plot distributions of ALL numeric measurements in one figure.
# We need the data in long format.

penguins_plot_data <- penguins |>
  select(species, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) |>
  drop_na() |>
  pivot_longer(
    cols      = -species,
    names_to  = "measurement",
    values_to = "value"
  )

head(penguins_plot_data)

# Now we can facet by measurement
ggplot(penguins_plot_data, aes(x = value, fill = species)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ measurement, scales = "free") +
  labs(title = "Distribution of measurements by species",
       x = NULL, y = "Density") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


# ==============================================================================
# PART 3 — HANDLING MISSING VALUES
# ==============================================================================

# ---- 3.1  Identify missing values -------------------------------------------
colSums(is.na(penguins))

# Which rows have missing sex?
penguins |>
  filter(is.na(sex))

# ---- 3.2  drop_na() — remove rows with ANY missing value --------------------
penguins_clean <- penguins |>
  drop_na()

nrow(penguins)        # before
nrow(penguins_clean)  # after

# Drop NA in specific columns only
penguins |>
  drop_na(sex, body_mass_g)

# ---- 3.3  replace_na() — fill missing values with a chosen value ------------
penguins |>
  mutate(sex = replace_na(sex, "unknown")) |>
  count(sex)

# ==============================================================================
# 🏠 HOMEWORK
# ==============================================================================
# 1. Filter penguins to only Gentoo species and select bill columns + body_mass_g
# 2. Create a summary table: mean and SD of each numeric variable, grouped by sex
# 3. Reshape that summary from wide to long using pivot_longer()
# 4. Try the same reshape using gather() — are the results identical?
# ==============================================================================
