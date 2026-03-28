# ==============================================================================
# Basic Data Manipulation – Tidyverse Edition
# Original tutorial: https://ourcodingclub.github.io/tutorials/data-manip-intro/
# Converted: base-R patterns replaced with tidyverse equivalents,
#            deprecated functions replaced with current alternatives.
# Plots skipped; data wrangling only.
# ==============================================================================

# LIBRARIES --------------------------------------------------------------------
library(tidyverse)   # loads dplyr, tidyr, forcats, stringr, purrr, readr, etc.


# ==============================================================================
# 1. SUBSET, EXTRACT AND MODIFY DATA
# ==============================================================================

# --- Load data ----------------------------------------------------------------
# read_csv() (readr) instead of base read.csv():
#   - returns a tibble (prints more cleanly)
#   - never converts strings to factors silently
#   - column types are guessed and reported on import

elongation <- read_csv("EmpetrumElongation.csv")

# glimpse() is the tidyverse alternative to str() / head():
# shows dimensions, column types and the first few values in one call
glimpse(elongation)


# --- Accessing columns --------------------------------------------------------
# Base R: elongation$Indiv
# Tidyverse: pull() extracts a single column as a plain vector
elongation |> pull(Indiv)

# Number of distinct shrubs — n_distinct() is the tidyverse equivalent of
# length(unique(...))
elongation |> pull(Indiv) |> n_distinct()


# --- Subsetting rows (base R recap, then tidyverse equivalent) ----------------

# Base R: elongation[elongation$Indiv == 603, ]
# Tidyverse:
elongation |> filter(Indiv == 603)

# Base R: elongation[elongation$Zone < 4, ]
elongation |> filter(Zone < 4)

# Base R: elongation[elongation$Zone <= 4, ]
elongation |> filter(Zone <= 4)

# Base R: elongation[!elongation$Zone >= 5, ]
elongation |> filter(!Zone >= 5)    # or equivalently: filter(Zone < 5)

# Two conditions (OR)
# Base R: elongation[elongation$Zone == 2 | elongation$Zone == 7, ]
elongation |> filter(Zone %in% c(2, 7))

# Two conditions (AND) — between() is a convenient shorthand for a numeric range
# Base R: elongation[elongation$Zone == 2 & elongation$Indiv %in% c(300:400), ]
elongation |> filter(Zone == 2, between(Indiv, 300, 400))


# ==============================================================================
# CHANGING VARIABLE NAMES
# ==============================================================================

# Base R used names() assignment — fragile because it relies on column position:
#   names(elong2)[1] <- "zone"
#
# Tidyverse: rename() uses New = Old syntax and works by name, not position.

elong2 <- elongation |>
  rename(zone  = Zone,
         ID    = Indiv)

glimpse(elong2)


# --- Correcting a single value ------------------------------------------------
# Base R option 1: elong2[1, 4] <- 5.7           (position-dependent, fragile)
# Base R option 2: elong2[elong2$ID == 373, ]$X2008 <- 5.7
#
# Tidyverse: mutate() + if_else() keeps everything in a pipe and is explicit
# about which rows change. if_else() is dplyr's stricter replacement for base
# ifelse(): it enforces that the TRUE and FALSE values share the same type.

elong2 <- elong2 |>
  mutate(X2008 = if_else(ID == 373, 5.7, X2008))


# ==============================================================================
# WORKING WITH FACTORS
# ==============================================================================

# --- Converting a column to a factor ------------------------------------------
# Base R: elong2$zone <- as.factor(elong2$zone)
# Tidyverse: stay inside mutate()

elong2 <- elong2 |>
  mutate(zone = as.factor(zone))

glimpse(elong2)     # zone is now a factor with 6 levels


# --- Renaming factor levels ---------------------------------------------------
# Base R used direct levels() assignment — error-prone because the new labels
# must be supplied in the same order as the existing levels:
#   levels(elong2$zone) <- c("A","B","C","D","E","F")
#
# Tidyverse: fct_recode() from forcats uses New = "Old" pairs, so the mapping
# is explicit and order-independent.

elong2 <- elong2 |>
  mutate(zone = fct_recode(zone,
    A = "2", B = "3", C = "4",
    D = "5", E = "6", F = "7"
  ))

elong2 |> pull(zone) |> levels()


# ==============================================================================
# 2. TIDY DATA — WIDE → LONG AND BACK
# ==============================================================================

# gather() and spread() were superseded in tidyr 1.0.0 (2019).
# The replacements are pivot_longer() and pivot_wider(), which have
# clearer, more consistent argument names.

# --- Wide → Long (gather → pivot_longer) -------------------------------------
# Original: gather(elongation, Year, Length, c(X2007:X2012))

elongation_long <- elongation |>
  pivot_longer(
    cols      = X2007:X2012,   # columns to pivot (can also use starts_with("X"))
    names_to  = "Year",        # name of the new key column
    values_to = "Length"       # name of the new value column
  )

glimpse(elongation_long)

# --- Long → Wide (spread → pivot_wider) --------------------------------------
# Original: spread(elongation_long, Year, Length)

elongation_wide <- elongation_long |>
  pivot_wider(
    names_from  = Year,    # column whose values become new column names
    values_from = Length   # column whose values fill the new columns
  )

glimpse(elongation_wide)


# ==============================================================================
# 3. CORE dplyr FUNCTIONS
# ==============================================================================

# --- 3a. rename() -------------------------------------------------------------
# (Already used above — keeping it here for tutorial structure)

elongation_long <- elongation_long |>
  rename(zone   = Zone,
         indiv  = Indiv,
         year   = Year,
         length = Length)

glimpse(elongation_long)


# --- 3b. filter() rows and select() columns -----------------------------------

# Keep zones 2 & 3 and years 2009–2011
elong_subset <- elongation_long |>
  filter(zone %in% c(2, 3),
         year %in% c("X2009", "X2010", "X2011"))

# between() for a numeric range (equivalent to length > 4 & length <= 6.5)
elong_subset_range <- elongation_long |>
  filter(between(length, 4, 6.5))

# select() — keep named columns; use - to drop
elong_no.zone <- elongation_long |>
  dplyr::select(-zone)

# select() can also rename and reorder on the fly
elong_renamed <- elongation_long |>
  dplyr::select(Year = year, Shrub.ID = indiv, Growth = length)


# --- 3c. mutate() — create new columns ----------------------------------------

# Total growth 2007–2012 on the wide-format object
elong_total <- elongation |>
  mutate(total.growth = X2007 + X2008 + X2009 + X2010 + X2011 + X2012)

glimpse(elong_total)


# --- 3d & 3e. group_by() + summarise() ----------------------------------------

# Total growth per individual across all years
summary_by_indiv <- elongation_long |>
  group_by(indiv) |>
  summarise(
    total.growth = sum(length),
    mean.growth  = mean(length),
    sd.growth    = sd(length)
  )

summary_by_indiv


# --- 3f. *_join() — merge datasets -------------------------------------------

treatments <- read_csv("EmpetrumTreatments.csv")   # assumes semicolon sep; adjust
# If the file is semicolon-separated: read_delim("EmpetrumTreatments.csv", delim = ";")

# left_join keeps all rows from elongation_long and adds matching treatment info.
# Column names differ between the two tables, so we specify the mapping explicitly.
experiment <- elongation_long |>
  left_join(treatments, by = c("indiv" = "Indiv", "zone" = "Zone"))

glimpse(experiment)


# ==============================================================================
# 4. CHALLENGE — DRAGONS DATA (data wrangling only, plots skipped)
# ==============================================================================

dragons <- read_csv("dragons.csv")
glimpse(dragons)

# --- Step 1: Rename paprika → turmeric ----------------------------------------
dragons <- dragons |>
  rename(turmeric = paprika)

# --- Step 2: Fix calibration error (tabasco / hungarian_horntail) -------------
# Tidyverse: mutate() + if_else() to change only the affected rows.
# if_else() is used instead of base ifelse() for type safety.

dragons <- dragons |>
  mutate(tabasco = if_else(species == "hungarian_horntail",
                           tabasco - 30,
                           tabasco))

# --- Step 3: Reshape wide → long with pivot_longer() -------------------------
# Original tutorial used: gather(dragons, key = 'spice', value = 'plume', ...)
# pivot_longer() is the current replacement for the superseded gather().

dragons_long <- dragons |>
  pivot_longer(
    cols      = c(tabasco, jalapeno, wasabi, turmeric),
    names_to  = "spice",
    values_to = "plume"
  )

# --- Step 4: Convert cm → m inside a mutate() ---------------------------------
dragons_long <- dragons_long |>
  mutate(plume.m = plume / 100)

glimpse(dragons_long)

# --- Step 5: Quick summary — which spice triggers the biggest plume? ----------
dragons_long |>
  group_by(spice) |>
  summarise(
    mean_plume_m = mean(plume.m, na.rm = TRUE),
    sd_plume_m   = sd(plume.m,   na.rm = TRUE)
  ) |>
  arrange(desc(mean_plume_m))

# --- Step 6: Per-species subsets (for reference; plots are skipped) -----------
horntail   <- dragons_long |> filter(species == "hungarian_horntail")
green      <- dragons_long |> filter(species == "welsh_green")
shortsnout <- dragons_long |> filter(species == "swedish_shortsnout")

# ==============================================================================
# END
# ==============================================================================
