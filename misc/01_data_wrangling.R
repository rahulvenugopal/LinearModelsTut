# ============================================================
#  Script 01 — Hadley's Verbs & Cleaning Data
#  Dataset : palmerpenguins::penguins
#  Packages: tidyverse, palmerpenguins
# ============================================================
#
#  By the end of this script you will be able to:
#    • Load and inspect a dataset
#    • Clean missing values
#    • Use the six core dplyr verbs:
#        filter(), select(), mutate(), arrange(),
#        summarise(), group_by()
#    • Chain steps together with the pipe  |>  (or %>%)
#
# ============================================================

library(tidyverse)
library(palmerpenguins)   # install.packages("palmerpenguins") if needed


# ── 0. First look at the data ────────────────────────────────────────────────

penguins          # print the tibble
glimpse(penguins) # column names, types, first values
?penguins         # open the help page — always read the docs!

# Key columns we'll use a lot:
#   species         – Adelie / Chinstrap / Gentoo
#   island          – Biscoe / Dream / Torgersen
#   bill_length_mm  – length of the bill (culmen)
#   bill_depth_mm   – depth (height) of the bill
#   flipper_length_mm
#   body_mass_g
#   sex             – female / male
#   year            – 2007 / 2008 / 2009


# ── 1. Cleaning: drop_na() ───────────────────────────────────────────────────

# How many rows have at least one missing value?
penguins |> summarise(n_missing = sum(!complete.cases(penguins)))

# Remove them — we'll work with a clean copy from here on
penguins_clean <- penguins |> drop_na()

nrow(penguins)       # 344 rows before
nrow(penguins_clean) # 333 rows after — 11 rows dropped


# ── 2. filter() — keep rows that match a condition ───────────────────────────

# Keep only Chinstrap penguins
penguins_clean |> filter(species == "Chinstrap")

# Keep penguins heavier than 4 kg  (body_mass_g is in grams)
penguins_clean |> filter(body_mass_g > 4000)

# Combine conditions with & (AND) or | (OR)
penguins_clean |> filter(species == "Adelie" & sex == "female")
penguins_clean |> filter(island == "Biscoe"  | island == "Dream")

# Handy shortcut for "is one of these values"
penguins_clean |> filter(island %in% c("Biscoe", "Dream"))

# Exercise 1 ✏️
# Filter for male Gentoo penguins observed in 2009.
# How many rows do you get?


# ── 3. select() — keep (or drop) columns ─────────────────────────────────────

# Keep only the columns you need
penguins_clean |> select(species, sex, body_mass_g)

# Drop columns with a minus sign
penguins_clean |> select(-year, -island)

# Handy helpers
penguins_clean |> select(starts_with("bill"))
penguins_clean |> select(ends_with("mm"))
penguins_clean |> select(where(is.numeric))

# Exercise 2 ✏️
# Select only the species column and all columns that end in "_mm".


# ── 4. mutate() — create or transform columns ─────────────────────────────────

# Convert body mass from grams → kilograms
penguins_clean |>
  mutate(body_mass_kg = body_mass_g / 1000) |>
  select(species, body_mass_g, body_mass_kg)

# Create a bill "ratio" — often a useful derived variable
penguins_clean |>
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) |>
  select(species, bill_length_mm, bill_depth_mm, bill_ratio)

# Recode a column: capitalise sex labels for nicer printing
penguins_clean |>
  mutate(sex_label = str_to_title(sex)) |>   # "female" → "Female"
  select(sex, sex_label)

# Use case_when() for more complex recoding
penguins_clean |>
  mutate(size_category = case_when(
    body_mass_g < 3500  ~ "Small",
    body_mass_g < 4500  ~ "Medium",
    TRUE                ~ "Large"   # everything else
  )) |>
  count(size_category)

# Exercise 3 ✏️
# Add a column called flipper_cm that converts flipper_length_mm to centimetres.


# ── 5. arrange() — sort rows ──────────────────────────────────────────────────

# Ascending (default)
penguins_clean |> arrange(body_mass_g) |> select(species, body_mass_g)

# Descending — which penguin is the heaviest?
penguins_clean |> arrange(desc(body_mass_g)) |> select(species, body_mass_g)

# Sort by multiple columns
penguins_clean |>
  arrange(species, desc(bill_length_mm)) |>
  select(species, bill_length_mm)

# Exercise 4 ✏️
# Find the five penguins with the shortest flippers.
# Hint: combine arrange() and slice_head(n = 5).


# ── 6. summarise() — collapse rows to a single summary ───────────────────────

# Overall averages
penguins_clean |>
  summarise(
    mean_bill_length = mean(bill_length_mm),
    mean_body_mass   = mean(body_mass_g),
    n                = n()
  )

# Exercise 5 ✏️
# Calculate the median and standard deviation of flipper_length_mm
# across ALL penguins.


# ── 7. group_by() + summarise() — summaries per group ────────────────────────

# The real power: summaries broken down by a grouping variable
penguins_clean |>
  group_by(species) |>
  summarise(
    mean_bill_length = mean(bill_length_mm),
    mean_body_mass   = mean(body_mass_g),
    n                = n()
  )

# Two grouping variables at once
penguins_clean |>
  group_by(species, sex) |>
  summarise(
    mean_bill_length = mean(bill_length_mm),
    .groups = "drop"   # good practice: always drop groups after summarising
  )

# Modern alternative: .by = argument (no need for group_by / ungroup)
penguins_clean |>
  summarise(
    mean_bill_length = mean(bill_length_mm),
    n                = n(),
    .by = c(species, sex)
  )

# Exercise 6 ✏️
# For each island, find:
#   • the number of penguins observed
#   • the average body mass
# Which island has the heaviest penguins on average?


# ── 8. Putting it all together — a wrangling pipeline ────────────────────────

# Task: "What is the average bill ratio for each species, sorted largest first?"

penguins_clean |>
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) |>
  group_by(species) |>
  summarise(
    avg_bill_ratio = mean(bill_ratio),
    n              = n(),
    .groups = "drop"
  ) |>
  arrange(desc(avg_bill_ratio))

# Read this pipeline out loud:
#   "Take penguins_clean, THEN
#    calculate bill_ratio, THEN
#    group by species, THEN
#    summarise to get the average, THEN
#    sort descending."
#
# That's the tidyverse way: short, readable, chainable steps.


# ── Final challenge 🏆 ────────────────────────────────────────────────────────
#
# Build a single pipeline that:
#   1. Starts from penguins_clean
#   2. Keeps only Adelie and Chinstrap penguins
#   3. Adds a column body_mass_kg (grams ÷ 1000)
#   4. Groups by species and sex
#   5. Summarises: mean body_mass_kg, min and max flipper_length_mm, count n
#   6. Sorts by mean body_mass_kg descending
#
# What pattern do you notice between species and sex?

