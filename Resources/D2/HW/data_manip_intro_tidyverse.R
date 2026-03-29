# ==============================================================================
# Basic Data Manipulation – Tidyverse Edition
# Original tutorial: https://ourcodingclub.github.io/tutorials/data-manip-intro/
# ==============================================================================

library(tidyverse)


# ==============================================================================
# 1. SUBSET, EXTRACT AND MODIFY DATA
# ==============================================================================

# Load data --------------------------------------------------------------------
# read_csv() returns a tibble, guesses column types, and never silently converts
# strings to factors
elongation <- read_csv("EmpetrumElongation.csv")

# glimpse() shows dimensions, column types, and first few values in one call
glimpse(elongation)


# Accessing columns ------------------------------------------------------------
# pull() extracts a single column as a plain vector
elongation |> pull(Indiv)

# n_distinct() counts the number of unique values in a vector
elongation |> pull(Indiv) |> n_distinct()


# Subsetting rows with filter() ------------------------------------------------

# Single condition
elongation |> filter(Indiv == 603)
elongation |> filter(Zone < 4)
elongation |> filter(Zone <= 4)
elongation |> filter(!Zone >= 5)    # equivalent: filter(Zone < 5)

# Match multiple values with %in%
elongation |> filter(Zone %in% c(2, 7))

# Multiple conditions — between() is a convenient shorthand for a numeric range
elongation |> filter(Zone == 2, between(Indiv, 300, 400))


# ==============================================================================
# CHANGING VARIABLE NAMES
# ==============================================================================

# rename() uses New = Old syntax and works by column name, not position
elong2 <- elongation |>
  rename(zone = Zone,
         ID   = Indiv)

glimpse(elong2)


# Correcting a single value ----------------------------------------------------
# mutate() + if_else() is explicit about which rows change.
# if_else() enforces that TRUE and FALSE values share the same type.
elong2 <- elong2 |>
  mutate(X2008 = if_else(ID == 373, 5.7, X2008))


# ==============================================================================
# WORKING WITH FACTORS
# ==============================================================================

# Converting a column to a factor inside mutate()
elong2 <- elong2 |>
  mutate(zone = as.factor(zone))

glimpse(elong2)    # zone is now a factor with 6 levels


# Renaming factor levels -------------------------------------------------------
# fct_recode() uses New = "Old" pairs — the mapping is explicit and
# order-independent, so it won't silently mis-assign labels
elong2 <- elong2 |>
  mutate(zone = fct_recode(zone,
    A = "2", B = "3", C = "4",
    D = "5", E = "6", F = "7"
  ))

elong2 |> pull(zone) |> levels()


# ==============================================================================
# 2. TIDY DATA — WIDE → LONG AND BACK
# ==============================================================================

# Wide → Long with pivot_longer() ----------------------------------------------
elongation_long <- elongation |>
  pivot_longer(
    cols      = X2007:X2012,   # columns to pivot (can also use starts_with("X"))
    names_to  = "Year",        # name of the new key column
    values_to = "Length"       # name of the new value column
  )

glimpse(elongation_long)


# Long → Wide with pivot_wider() -----------------------------------------------
elongation_wide <- elongation_long |>
  pivot_wider(
    names_from  = Year,    # column whose values become new column names
    values_from = Length   # column whose values fill the new columns
  )

glimpse(elongation_wide)


# ==============================================================================
# 3. CORE dplyr FUNCTIONS
# ==============================================================================

# 3a. rename() -----------------------------------------------------------------
elongation_long <- elongation_long |>
  rename(zone   = Zone,
         indiv  = Indiv,
         year   = Year,
         length = Length)

glimpse(elongation_long)


# 3b. filter() rows and select() columns ---------------------------------------

# Keep zones 2 & 3 and years 2009–2011
elong_subset <- elongation_long |>
  filter(zone %in% c(2, 3),
         year %in% c("X2009", "X2010", "X2011"))

# between() for a numeric range
elong_subset_range <- elongation_long |>
  filter(between(length, 4, 6.5))

# Use - to drop columns
elong_no.zone <- elongation_long |>
  dplyr::select(-zone)

# select() can also rename and reorder on the fly
elong_renamed <- elongation_long |>
  dplyr::select(Year = year, Shrub.ID = indiv, Growth = length)


# 3c. mutate() — create new columns -------------------------------------------
elong_total <- elongation |>
  mutate(total.growth = X2007 + X2008 + X2009 + X2010 + X2011 + X2012)

glimpse(elong_total)


# 3d & 3e. group_by() + summarise() -------------------------------------------
summary_by_indiv <- elongation_long |>
  group_by(indiv) |>
  summarise(
    total.growth = sum(length),
    mean.growth  = mean(length),
    sd.growth    = sd(length)
  )

summary_by_indiv


# 3f. left_join() — merge datasets --------------------------------------------
# Column names differ between the two tables, so we specify the mapping explicitly
treatments <- read_csv("EmpetrumTreatments.csv")

experiment <- elongation_long |>
  left_join(treatments, by = c("indiv" = "Indiv", "zone" = "Zone"))

glimpse(experiment)


# ==============================================================================
# 4. CHALLENGE — DRAGONS DATA
# ==============================================================================

# QUESTION:
# Using the dragons dataset:
#   1. Rename the column 'paprika' to 'turmeric'
#   2. Fix a calibration error: subtract 30 from the 'tabasco' column for
#      hungarian_horntail species only
#   3. Reshape from wide to long so each row is one spice measurement per dragon
#   4. Convert the 'plume' column from cm to metres (new column: plume.m)
#   5. Summarise: which spice triggers the largest mean plume height?

dragons <- read_csv("dragons.csv")
glimpse(dragons)

# Step 1: Rename paprika → turmeric
dragons <- dragons |>
  rename(turmeric = paprika)

# Step 2: Fix calibration error for hungarian_horntail rows only
dragons <- dragons |>
  mutate(tabasco = if_else(species == "hungarian_horntail",
                           tabasco - 30,
                           tabasco))

# Step 3: Reshape wide → long across the four spice columns
dragons_long <- dragons |>
  pivot_longer(
    cols      = c(tabasco, jalapeno, wasabi, turmeric),
    names_to  = "spice",
    values_to = "plume"
  )

# Step 4: Convert cm → m
dragons_long <- dragons_long |>
  mutate(plume.m = plume / 100)

glimpse(dragons_long)

# Step 5: Summarise by spice, sorted by mean plume height descending
dragons_long |>
  group_by(spice) |>
  summarise(
    mean_plume_m = mean(plume.m, na.rm = TRUE),
    sd_plume_m   = sd(plume.m,   na.rm = TRUE)
  ) |>
  arrange(desc(mean_plume_m))

# Per-species subsets (for further analysis or plotting)
horntail   <- dragons_long |> filter(species == "hungarian_horntail")
green      <- dragons_long |> filter(species == "welsh_green")
shortsnout <- dragons_long |> filter(species == "swedish_shortsnout")