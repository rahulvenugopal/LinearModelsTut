# ==============================================================================
# Efficient Data Manipulation – Tidyverse Edition
# Original tutorial: https://ourcodingclub.github.io/tutorials/data-manip-efficient/
# Converted: all base-R patterns replaced with tidyverse equivalents
# Plots skipped; data-wrangling only
# ==============================================================================

# LIBRARIES --------------------------------------------------------------------
library(tidyverse)   # loads dplyr, tidyr, forcats, stringr, purrr, readr, etc.

# LOAD DATA --------------------------------------------------------------------
trees <- read_csv("trees.csv")   # readr::read_csv instead of base read.csv

glimpse(trees)   # tidyverse alternative to head(); shows column types too


# ==============================================================================
# 1. AN INTRODUCTION TO PIPES
# ==============================================================================

# --- Old way (base dplyr, creating an intermediate object) --------------------
trees.grouped <- group_by(trees, CommonName)
trees.summary <- tally(trees.grouped)

# --- Tidyverse pipe way -------------------------------------------------------
trees.summary <- trees %>%
  group_by(CommonName) %>%
  tally()                      # or use count(CommonName) — see below

# count() is a convenient shortcut that combines group_by + tally in one step:
trees.summary <- trees %>%
  count(CommonName, name = "count")

# --- Filter then count by two grouping variables ------------------------------
trees.subset <- trees %>%
  filter(CommonName %in% c("Common Ash", "Rowan", "Scots Pine")) %>%
  count(CommonName, AgeGroup)


# ==============================================================================
# 2a. summarise_all() / across() — quickly summarise all columns
# ==============================================================================

# summarise_all() is superseded; the modern tidyverse approach uses across()
# Note: passing extra args via ... to across() is deprecated since dplyr 1.1.0;
# use an anonymous lambda \(x) instead.
summ.all <- trees %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

# If you want all columns attempted (non-numeric become NA), you can still do:
summ.all.all <- trees %>%
  summarise(across(everything(), \(x) mean(as.numeric(x), na.rm = TRUE)))


# ==============================================================================
# 2b. case_when() — reclassifying values
# ==============================================================================

# Simple if_else demo — dplyr's if_else() is the tidyverse replacement for
# base ifelse(); it is stricter about types (true/false values must match),
# which prevents silent type coercion bugs.
vector <- c(4, 13, 15, 6)
if_else(vector < 10, "A", "B")

# case_when demo (unchanged — already tidy)
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

# --- See all unique species (tidyverse style) ---------------------------------
trees %>% distinct(LatinName)   # returns a data frame, not just a vector
# or: pull(trees, LatinName) %>% unique() — if you need a plain vector

# --- Create Genus column with str_detect() + case_when() + mutate() ---------
# str_detect() from stringr replaces base grepl() — consistent with the
# tidyverse, works naturally inside mutate() and supports regex by default.
trees.genus <- trees %>%
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

# --- BONUS: faster approach using separate_wider_delim() ---------------------
# tidyr::separate() was superseded in tidyr 1.3.0 by the *_wider_* family.
# separate_wider_delim() splits on a literal delimiter string.
trees.genus.2 <- trees %>%
  separate_wider_delim(
    LatinName,
    delim   = " ",
    names   = c("Genus", "Species"),
    cols_remove = FALSE          # keeps the original LatinName column
  ) %>%
  select(-Species)

# --- Reclassify Height into 3 categories -------------------------------------
trees.genus <- trees.genus %>%
  mutate(Height.cat = case_when(
    Height %in% c("Up to 5 meters", "5 to 10 meters")       ~ "Short",
    Height %in% c("10 to 15 meters", "15 to 20 meters")     ~ "Medium",
    Height == "20 to 25 meters"                              ~ "Tall"
  ))

# --- Reorder AND relabel factor levels with forcats (tidyverse) ---------------
# Base R way used direct assignment:  trees.genus$Height.cat <- factor(...)
# Tidyverse way: stay inside mutate() using forcats functions

# Check current levels
trees.genus %>% pull(Height.cat) %>% levels()

# Reorder levels and relabel, all inside a pipe
trees.genus <- trees.genus %>%
  mutate(Height.cat = fct_relevel(Height.cat, "Short", "Medium", "Tall")) %>%
  mutate(Height.cat = fct_recode(Height.cat,
    "SHORT"  = "Short",
    "MEDIUM" = "Medium",
    "TALL"   = "Tall"
  ))

# Verify new order and labels
trees.genus %>% pull(Height.cat) %>% levels()


# ==============================================================================
# 4. ADVANCED PIPING (data wrangling side only; plots skipped)
# ==============================================================================

# Subset to five genera
trees.five <- trees.genus %>%
  filter(Genus %in% c("Acer", "Fraxinus", "Salix", "Aesculus", "Pinus"))


# ==============================================================================
# 5. CHALLENGE — QUADRANT ANALYSIS (data wrangling only)
# ==============================================================================

# --- Step 1: Create quadrants ------------------------------------------------
lon <- (max(trees.genus$Easting)  - min(trees.genus$Easting))  / 2 + min(trees.genus$Easting)
lat <- (max(trees.genus$Northing) - min(trees.genus$Northing)) / 2 + min(trees.genus$Northing)

trees.genus <- trees.genus %>%
  mutate(Quadrant = case_when(
    Easting <= lon & Northing >  lat ~ "NW",
    Easting <= lon & Northing <= lat ~ "SW",    # <= on lat too to catch any exact midpoints
    Easting >  lon & Northing >  lat ~ "NE",
    Easting >  lon & Northing <= lat ~ "SE"
  ))

# Quick check — no NAs expected
trees.genus %>% count(Quadrant)

# --- Challenge Q1: Species richness per quadrant -----------------------------
# Base R used length(unique(...)); tidyverse uses n_distinct()

sp.richness <- trees.genus %>%
  group_by(Quadrant) %>%
  summarise(richness = n_distinct(LatinName))

sp.richness

# --- Challenge Q2: Proportion of Acer per quadrant ---------------------------
acer.percent <- trees.genus %>%
  count(Quadrant, Genus) %>%               # count trees per quadrant × genus
  group_by(Quadrant) %>%
  mutate(total   = sum(n),
         percent = n / total) %>%
  filter(Genus == "Acer") %>%
  ungroup()

acer.percent

# --- Challenge Q3: Acer data with collapsed & reordered age factor -----------
# Base R used direct $ assignment to modify the factor;
# tidyverse stays inside mutate() with fct_collapse() + fct_relevel()

acer <- trees.genus %>%
  filter(Genus == "Acer") %>%
  mutate(AgeGroup = fct_collapse(AgeGroup,
    "Young"      = c("Juvenile", "Semi-mature"),
    "Middle Aged" = "Middle Aged",
    "Mature"     = "Mature"
  )) %>%
  mutate(AgeGroup = fct_relevel(AgeGroup, "Young", "Middle Aged", "Mature"))

# Verify the levels
acer %>% pull(AgeGroup) %>% levels()

# Quick count to see the distribution per quadrant
acer %>%
  count(Quadrant, AgeGroup)

# ==============================================================================
# END
# ==============================================================================
