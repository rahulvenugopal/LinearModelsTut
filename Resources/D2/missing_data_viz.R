# Load libraries
library(tidyverse)
library(palmerpenguins)
library(naniar)    # Specialized library for missing data summaries & plots
library(visdat)    # Specialized library for visualizing whole dataframes

total_nas <- sum(is.na(penguins))

# Which columns have missing values, and exactly how many?
# colSums is a fast base R function that counts TRUE values (NAs) per column
na_by_column <- colSums(is.na(penguins))

# Let's use naniar's miss_var_summary() to get a clean, tidy dataframe of NAs
# This gives us both the count and the percentage of missingness per column.
missing_summary <- miss_var_summary(penguins)

# Visualization 1: The "Bird's Eye View" using visdat
# vis_miss() draws a map of your entire dataframe. 
# Rows are rows, columns are columns. Black lines indicate missing data.
# It also prints the overall percentage of missing data on the plot.
vis_miss(penguins)

# Visualization 2: The Bar Chart using naniar
# gg_miss_var() creates a clean ggplot bar chart showing the number of 
# missing values in each variable. It makes it instantly obvious which 
# columns are problematic.
gg_miss_var(penguins) + 
  labs(title = "Missing Values in the Palmer Penguins Dataset",
       y = "Number of Missing Values (NAs)")

# Visualization 3: Pattern Exploration
# gg_miss_upset() shows combinations of missingness.
# For example, if a penguin is missing its bill length, is it ALWAYS 
# missing its body mass too? This plot answers that.
gg_miss_upset(penguins)

# Cleanup
# drop_na() from the tidyr package removes any row containing an NA.
penguins_clean <- penguins %>%
  drop_na()