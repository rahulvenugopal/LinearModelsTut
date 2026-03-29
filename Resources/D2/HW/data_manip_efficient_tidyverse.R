# ==============================================================================
# Efficient Data Manipulation – Tidyverse Edition
# Original tutorial: https://ourcodingclub.github.io/tutorials/data-manip-efficient/
# ==============================================================================

library(tidyverse)

# Load data --------------------------------------------------------------------
trees <- read_csv("data/trees.csv")
glimpse(trees)    # shows column types as well as first few values


# ==============================================================================
# 1. PIPES AND COUNTING
# ==============================================================================

# count() combines group_by + tally in a single step
trees.summary <- trees |>
  count(CommonName, name = "count")

# Filter then count across two grouping variables
trees.subset <- trees |>
  filter(CommonName %in% c("Common Ash", "Rowan", "Scots Pine")) |>
  count(CommonName, AgeGroup)


# ==============================================================================
# 2a. across() — summarise multiple columns at once
# ==============================================================================

# Use an anonymous lambda \(x) when passing extra arguments to across()
summ.all <- trees |>
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))


# ==============================================================================
# 2b. if_else() and case_when() — reclassifying values
# ==============================================================================

# if_else() is strict about types: TRUE and FALSE values must be the same type,
# which prevents silent type-coercion bugs
vector <- c(4, 13, 15, 6)
if_else(vector < 10, "A", "B")

# case_when() handles multiple conditions cleanly
vector2 <- c("What am I?", "A", "B", "C", "D")
case_when(
  vector2 == "What am I?" ~ "I am the walrus",
  vector2 %in% c("A", "B") ~ "goo",
  vector2 == "C" ~ "ga",
  vector2 == "D" ~ "joob"
)


# ==============================================================================
# 3. CHANGING FACTOR LEVELS / CREATING CATEGORICAL VARIABLES
# ==============================================================================

# distinct() returns a data frame of unique values
trees |> distinct(LatinName)

# Create a Genus column using str_detect() inside case_when()
# str_detect() works naturally inside mutate() and supports regex by default
trees.genus <- trees |>
  mutate(Genus = case_when(
    str_detect(LatinName, "Acer")      ~ "Acer",
    str_detect(LatinName, "Fraxinus")  ~ "Fraxinus",
    str_detect(LatinName, "Sorbus")    ~ "Sorbus",
    str_detect(LatinName, "Betula")    ~ "Betula",
    str_detect(LatinName, "Populus")   ~ "Populus",
    str_detect(LatinName, "Laburnum")  ~ "Laburnum",
    str_detect(LatinName, "Aesculus")  ~ "Aesculus",
    str_detect(LatinName, "Fagus")     ~ "Fagus",
    str_detect(LatinName, "Prunus")    ~ "Prunus",
    str_detect(LatinName, "Pinus")     ~ "Pinus",
    str_detect(LatinName, "Sambucus")  ~ "Sambucus",
    str_detect(LatinName, "Crataegus") ~ "Crataegus",
    str_detect(LatinName, "Ilex")      ~ "Ilex",
    str_detect(LatinName, "Quercus")   ~ "Quercus",
    str_detect(LatinName, "Larix")     ~ "Larix",
    str_detect(LatinName, "Salix")     ~ "Salix",
    str_detect(LatinName, "Alnus")     ~ "Alnus"
  ))

# BONUS: faster approach — split LatinName on a space to get Genus directly.
# separate_wider_delim() splits on a literal delimiter string;
# cols_remove = FALSE retains the original column alongside the new ones.
trees.genus.2 <- trees |>
  separate_wider_delim(
    LatinName,
    delim       = " ",
    names       = c("Genus", "Species"),
    cols_remove = FALSE
  ) |>
  select(-Species)


# Reclassify Height into three broad categories
trees.genus <- trees.genus |>
  mutate(Height.cat = case_when(
    Height %in% c("Up to 5 meters", "5 to 10 meters")       ~ "Short",
    Height %in% c("10 to 15 meters", "15 to 20 meters")     ~ "Medium",
    Height == "20 to 25 meters"                              ~ "Tall"
  ))

# Check current factor levels before reordering
trees.genus |> pull(Height.cat) |> levels()

# fct_relevel() sets a custom order; fct_recode() renames levels — both stay
# inside mutate() so there is no out-of-pipe assignment
trees.genus <- trees.genus |>
  mutate(Height.cat = fct_relevel(Height.cat, "Short", "Medium", "Tall")) |>
  mutate(Height.cat = fct_recode(Height.cat,
    "SHORT"  = "Short",
    "MEDIUM" = "Medium",
    "TALL"   = "Tall"
  ))

# Verify new order and labels
trees.genus |> pull(Height.cat) |> levels()


# ==============================================================================
# 4. FILTERING TO A SUBSET OF GENERA
# ==============================================================================

trees.five <- trees.genus |>
  filter(Genus %in% c("Acer", "Fraxinus", "Salix", "Aesculus", "Pinus"))


# ==============================================================================
# 5. CHALLENGE — QUADRANT ANALYSIS
# ==============================================================================

# QUESTION:
# 1. Divide the trees into four spatial quadrants (NW, NE, SW, SE) based on
#    the midpoint of the Easting and Northing columns.
# 2. Calculate species richness (number of distinct species) per quadrant.
# 3. Calculate the proportion of Acer trees within each quadrant.
# 4. Filter to Acer only and collapse AgeGroup into three levels:
#    "Young" (Juvenile + Semi-mature), "Middle Aged", and "Mature",
#    then reorder those levels logically.

# Step 1: Calculate midpoints and assign quadrants with case_when()
lon <- (max(trees.genus$Easting)  - min(trees.genus$Easting))  / 2 + min(trees.genus$Easting)
lat <- (max(trees.genus$Northing) - min(trees.genus$Northing)) / 2 + min(trees.genus$Northing)

trees.genus <- trees.genus |>
  mutate(Quadrant = case_when(
    Easting <= lon & Northing >  lat ~ "NW",
    Easting <= lon & Northing <= lat ~ "SW",
    Easting >  lon & Northing >  lat ~ "NE",
    Easting >  lon & Northing <= lat ~ "SE"
  ))

# Quick check — no NAs expected
trees.genus |> count(Quadrant)


# Step 2: Species richness per quadrant
# n_distinct() counts unique values within each group
sp.richness <- trees.genus |>
  group_by(Quadrant) |>
  summarise(richness = n_distinct(LatinName))

sp.richness


# Step 3: Proportion of Acer per quadrant
acer.percent <- trees.genus |>
  count(Quadrant, Genus) |>              # count trees per quadrant × genus
  group_by(Quadrant) |>
  mutate(total   = sum(n),
         percent = n / total) |>
  filter(Genus == "Acer") |>
  ungroup()

acer.percent


# Step 4: Acer subset with collapsed and reordered age factor
# fct_collapse() merges existing levels into broader groups;
# fct_relevel() then sets the logical display order
acer <- trees.genus |>
  filter(Genus == "Acer") |>
  mutate(AgeGroup = fct_collapse(AgeGroup,
    "Young"       = c("Juvenile", "Semi-mature"),
    "Middle Aged" = "Middle Aged",
    "Mature"      = "Mature"
  )) |>
  mutate(AgeGroup = fct_relevel(AgeGroup, "Young", "Middle Aged", "Mature"))

# Verify the levels
acer |> pull(AgeGroup) |> levels()

# Distribution per quadrant
acer |>
  count(Quadrant, AgeGroup)