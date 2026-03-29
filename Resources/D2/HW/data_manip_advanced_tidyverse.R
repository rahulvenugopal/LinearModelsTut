# ==============================================================================
# Advanced Data Manipulation – Tidyverse Edition
# Original tutorial: https://ourcodingclub.github.io/tutorials/data-manip-creative-dplyr/
# ==============================================================================

library(tidyverse)


# ==============================================================================
# PART I: OCEAN ANIMALS
# ==============================================================================

# Load datasets ----------------------------------------------------------------
animal_p1   <- read_csv("data/animal_p1.csv")
animal_p2   <- read_csv("data/animal_p2.csv")
animal_rp   <- read_csv("data/animal_rp.csv")
animal_meal <- read_csv("data/animal_meal.csv")


# ==============================================================================
# 2a. Combining rows with bind_rows()
# ==============================================================================

animal_p1
animal_p2

# bind_rows() stacks data frames that share the same columns
animal <- bind_rows(animal_p1, animal_p2)   # 8 observations
animal


# ==============================================================================
# 2b. Set operations — comparing datasets
# ==============================================================================

setequal(animal_p1, animal_p2)         # FALSE — tables are not identical

intersect(animal, animal_rp)           # rows present in both

setdiff(animal, animal_rp)             # in animal but not animal_rp
setdiff(animal_rp, animal)             # in animal_rp but not animal

# union() merges both and removes exact duplicate rows; arrange() orders by id
animal_weight <- union(animal, animal_rp) |>
  arrange(id)

animal_weight   # 10 unique observations


# ==============================================================================
# 2c. Mutating joins
# ==============================================================================

animal_meal   # 21 observations — more meals than animals

# left_join keeps every row from animal_weight and adds matching meal info.
# Specifying by = explicitly is best practice even when names match.
animal_joined <- animal_weight |>
  left_join(animal_meal, by = c("id" = "IDs"))

animal_joined   # turtle id == 2 gets NA (no match in animal_meal)

# Other join types for comparison (printed to console, not saved):
inner_join(animal_weight, animal_meal, by = c("id" = "IDs"))  # drops unmatched rows
right_join(animal_weight, animal_meal, by = c("id" = "IDs"))  # all meal ids retained
full_join(animal_weight,  animal_meal, by = c("id" = "IDs"))  # everything kept

# When using full_join() to stack rows, specify ALL shared columns to avoid
# columns being placed side-by-side instead of stacked
full_join(animal_p1, animal_p2, by = c("id", "animal", "weight"))


# ==============================================================================
# 2d. Filtering joins
# ==============================================================================

# semi_join: keep rows in x that have a match in y (no columns added from y)
semi_join(animal_weight, animal_meal, by = c("id" = "IDs"))

# anti_join: keep rows in x that do NOT have a match in y
anti_join(animal_weight, animal_meal, by = c("id" = "IDs"))


# ==============================================================================
# 2e. CHALLENGE — combine animal_new with animal_joined
# ==============================================================================

# QUESTION:
# Load animal_new.csv and join it with animal_joined so that every animal
# (including any new ones) and all their meal records are retained.
# The two tables use different column names — map them correctly in by = .

animal_new <- read_csv("data/animal_new.csv")
glimpse(animal_new)

# full_join handles different column names and different column order cleanly
animal_final <- animal_joined |>
  full_join(animal_new,
            by = c("id" = "ID", "animal" = "Animals", "weight", "meal" = "Meal"))

animal_final


# ==============================================================================
# PART II: OCEANIA LPI DATA
# ==============================================================================

# Load and tidy the LPI dataset ------------------------------------------------
marine <- read_csv("data/LPI_marine.csv")

# Tidy: wide → long, clean year column, coerce pop to numeric, drop NAs
# parse_number() strips the leading "X" from year column names ("X2007" → 2007)
marine2 <- marine |>
  pivot_longer(
    cols      = 25:69,          # year columns to pivot
    names_to  = "year",
    values_to = "pop"
  ) |>
  mutate(
    year = parse_number(year),
    pop  = as.numeric(pop)
  ) |>
  drop_na(pop)

glimpse(marine2)


# ==============================================================================
# 3a. Extracting variables: select() and helpers
# ==============================================================================

# pull() → plain vector;  select() → single-column tibble
marine2 |> pull(Species)  |> glimpse()
marine2 |> select(Species) |> glimpse()

# Keep specific columns in a chosen order
marine2 |>
  select(id, pop, year, Country.list) |>
  glimpse()

# Rename on the fly inside select() using new_name = old_name
marine2 |>
  select("Country list" = Country.list,
         method         = Sampling.method) |>
  glimpse()

# Bring chosen columns to the front; keep the rest with everything()
marine2 |>
  select(id, year, pop, everything()) |>
  glimpse()

# Column ranges (by name or position)
marine2 |>
  select(Family:Species, 24:26) |>
  glimpse()

# Drop columns with -
marine2 |>
  select(-c(2:22, 24)) |>
  glimpse()

# all_of() selects from a pre-defined character vector safely
marine_cols <- c("Genus", "Species", "year", "pop", "id")

marine2 |>
  select(all_of(marine_cols)) |>
  glimpse()

# Selector helpers: starts_with(), ends_with(), contains()
marine2 |>
  select(starts_with("Decimal")) |>
  glimpse()

# select(where(predicate)) selects by column type
marine2 |>
  select(where(is.numeric)) |>
  glimpse()

# Mixing selectors in one select() call
marine2 |>
  select(id,
         Class:Family,
         genus = Genus,
         starts_with("Decimal"),
         everything(),
         -c(6:9, system:Data.transformed)) |>
  glimpse()

# Save the trimmed dataset we'll carry forward
marine3 <- marine2 |>
  select(id, Class, Genus, Species, year, pop,
         location = Location.of.population,
         lat      = Decimal.Latitude,
         lon      = Decimal.Longitude)

glimpse(marine3)


# ==============================================================================
# 3b. Renaming variables: rename() and rename_with()
# ==============================================================================

# rename() — individual columns by name
marine3 |>
  rename(class   = Class,
         genus   = Genus,
         species = Species) |>
  glimpse()

# rename_with() applies a function to column names.
# Rename ALL columns to lowercase
marine3 |>
  rename_with(tolower) |>
  glimpse()

# Rename only CHOSEN columns
marine3 |>
  rename_with(tolower, .cols = c(Genus, Species)) |>
  glimpse()

# Save fully lowercase version
marine4 <- marine3 |>
  rename_with(tolower)

glimpse(marine4)


# ==============================================================================
# 3c. Making new variables: mutate() and helpers
# ==============================================================================

# paste() to combine two columns
marine5 <- marine4 |>
  mutate(genus_species = paste(genus, species, sep = "_"))

glimpse(marine5)

# case_when() for multi-condition new columns
marine6 <- marine5 |>
  mutate(region = case_when(
    lat >  0 & lon >= 0 ~ "NE",
    lat <= 0 & lon >= 0 ~ "SE",
    lat >  0 & lon <  0 ~ "NW",
    lat <= 0 & lon <  0 ~ "SW"
  ))

marine6 |> distinct(region)

# mutate(.keep = "none") drops all columns not created or mentioned in the call —
# useful when you only want the derived columns
marine4 |>
  mutate(
    .keep         = "none",
    genus_species = paste(genus, species, sep = "_"),
    region        = case_when(
      lat >  0 & lon >= 0 ~ "NE",
      lat <= 0 & lon >= 0 ~ "SE",
      lat >  0 & lon <  0 ~ "NW",
      lat <= 0 & lon <  0 ~ "SW"
    )
  ) |>
  glimpse()

# mutate(across()) applies a function to multiple columns at once
# Contrast with rename_with(): rename_with() changes column *names*;
# mutate(across()) changes column *values*
marine6 |>
  mutate(across(c(class, genus, location), tolower)) |>
  glimpse()

# add_column() inserts a new column at any position
marine6 |>
  add_column(observation_num = seq_len(nrow(marine6))) |>
  glimpse()

# add_tally() adds the group size as a new column without collapsing rows
marine6 |>
  select(genus_species, year) |>
  group_by(genus_species) |>
  add_tally(name = "observations_count") |>
  glimpse()

# add_count() is a shortcut: no need to call group_by() first
marine6 |>
  select(genus_species, year) |>
  add_count(genus_species, name = "observations_count") |>
  glimpse()


# ==============================================================================
# 4a. Extracting cases: filter() and helpers
# ==============================================================================

# Single condition
marine6 |> filter(class == "Mammalia") |> glimpse()

# Match multiple values
marine6 |> filter(class %in% c("Mammalia", "Aves")) |> glimpse()

# Equivalent using |
marine6 |> filter(class == "Mammalia" | class == "Aves") |> glimpse()

# Exclude with !
marine6 |> filter(class != "Actinopteri") |> glimpse()
marine6 |> filter(!class %in% c("Mammalia", "Aves")) |> glimpse()

# Numeric range — between() is a clean shorthand
marine6 |> filter(pop >= 10 & pop <= 100) |> glimpse()
marine6 |> filter(between(pop, 10, 100))  |> glimpse()

# Drop NAs
marine6 |> filter(!is.na(pop)) |> glimpse()

# Operator precedence matters — brackets change the meaning:
marine6 |>
  filter((class == "Mammalia" | pop > 100) & region != "SE") |>
  glimpse()   # Mammalia OR pop > 100, AND not SE → 38 rows

marine6 |>
  filter(class == "Mammalia" | (pop > 100 & region != "SE")) |>
  glimpse()   # Mammalia, OR (pop > 100 AND not SE) → 96 rows

# distinct() drops exact duplicate rows
marine6 |> distinct() |> glimpse()

# n_distinct() counts the number of distinct rows
marine6 |> n_distinct()

# slice() keeps specific row positions
marine6 |>
  select(id:species) |>
  slice(2:4)

# slice_max() returns the n rows with the highest values for a chosen column
marine6 |>
  slice_max(pop, n = 5) |>
  glimpse()


# ==============================================================================
# 4b. Adding cases: add_row()
# ==============================================================================

# QUESTION:
# Filter marine6 to species id 2077 (Chrysophrys auratus), keeping only
# id, genus_species, year, and pop. Then:
#   a) Add a row for 1997 (pop = 39 000) at the bottom of the dataset
#   b) Add a row for 1969 (pop = 39 000) as the very first row

marine7 <- marine6 |>
  filter(id == "2077") |>
  select(id, genus_species, year, pop)

marine7   # 27 observations for 1970–1996

# a) Append a row at the bottom
marine7 |>
  add_row(id = 2077, genus_species = "Chrysophrys_auratus", year = 1997, pop = 39000)

# b) Insert a row at position 1 with .before
marine7 |>
  add_row(id = 2077, genus_species = "Chrysophrys_auratus", year = 1969, pop = 39000,
          .before = 1)


# ==============================================================================
# 5. EXTRA: Green Sea Turtle — data prep
# ==============================================================================

# Filter to a single species and convert id to a factor for discrete grouping
marine_final <- marine6 |>
  filter(genus_species == "Chelonia_mydas") |>
  mutate(id = as.factor(id))

glimpse(marine_final)