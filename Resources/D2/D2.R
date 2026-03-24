# Load the libraries
library(tidyverse)
library(palmerpenguins) # Install if not there

# Walk through the data
glimpse(penguins)

# Hadley's verbs
penguins_Biscoe <- penguins %>%
  filter(island == "Biscoe") %>% # only from Biscoe island
  
  # # select(): Keep, drop, or reorder specific columns.
  select(species, sex, body_mass_g, flipper_length_mm) %>%
  
  # Change column names without dropping anything.
  rename(mass = body_mass_g, flipper_length = flipper_length_mm) %>%
  
  # Create new columns or modify existing ones.
  mutate(mass_kg = mass / 1000)

# group_by() and summarize(): The dynamic duo of data aggregation.
penguins_summary <- penguins %>%
  # Drop all rows having NA
  drop_na(body_mass_g, sex) %>% 
  # Group now
  group_by(species, sex) %>%
  # Summarise
  summarize(
    mean_mass = mean(body_mass_g),
    max_mass = max(body_mass_g),
    sd_mass = sd(body_mass_g),
    .groups = "drop" # Good practice to ungroup after summarizing
  )

# count(): The fastest way to see how many observations fall into categories.
# This does the grouping and tallying behind the scenes.
penguins %>%
  count(species, sex, island, name = "number_of_penguins")

# tally(): The manual equivalent to count(). You must group_by() first.
penguins %>%
  group_by(species, island) %>%
  tally(name = "number_of_penguins")

# Reshape the data

penguins_long <- penguins %>%
  select(species, island, bill_length_mm, bill_depth_mm) %>%
  pivot_longer(
    cols = c(bill_length_mm, bill_depth_mm),
    names_to = "bill_dimension",
    values_to = "measurement_mm"
  )

penguins_wide <- penguins_long %>%
  pivot_wider(
    names_from = bill_dimension,
    values_from = measurement_mm
  )