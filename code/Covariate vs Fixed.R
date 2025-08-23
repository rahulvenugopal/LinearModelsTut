# Show the difference between normal regression, covariate

# author @ Rahul Venugopal on August 8th 2025

# Load libraries
library(tidyverse)
library(palmerpenguins)
library(skimr)
library(psych)
library(ggthemes)
library(easystats)
library(patchwork)

# Dataset
penguins_clean <- penguins %>% 
  filter(!is.na(body_mass_g), !is.na(flipper_length_mm), !is.na(species)) %>% 
  select(body_mass_g, flipper_length_mm, species)

skim(penguins_clean)
describe(penguins_clean)

# Set the theme
my_theme <- theme_fivethirtyeight(base_family = "Fira Sans") +
  theme(
    plot.title = ggtext::element_markdown(size=18),
    legend.position = "none",
    axis.title.y = element_text(angle = 90, size = 14, face = 'bold'),
    axis.title.x = element_text(size = 14, face = 'bold'),
    plot.title.position = "plot",
    panel.background = element_rect(fill = "white",
                                    colour = "grey80"),
    plot.background = element_rect(fill = "white",
                                   colour = "grey80"),
    axis.ticks = element_line(color = "grey40"),
    axis.line = element_line(color = "grey60"),
    panel.grid.major = element_blank(),  # removes both x and y major gridlines
    panel.grid.minor = element_blank(),   # removes both x and y minor gridlines
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    axis.text = element_text(color = "grey20", size = 14, face = 'bold')
  )

theme_set(my_theme)


# Simpson's paradox -------------------------------------------------------

p1 <- ggplot(penguins, 
       aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Penguin beak dimensions",
       y = "Bill length (mm)",
       x = "Bill depth (mm)")

p2 <- ggplot(penguins, 
       aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Penguin beak dimensions",
       y = "Bill length (mm)",
       x = "Bill depth (mm)")

p1 + p2

# Visuals
# Case A plot
ggplot(penguins_clean, aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.5) +
  labs(title = "Body mass across species",
       y = "Body mass (g)")

ggsave('Bodymass_species.jpg',
       width = 6, height = 6, dpi = 600)

# Case B plot
ggplot(penguins_clean, 
       aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Flipper length in each species",
       y = "Body mass (g)",
       x = "Flipper length (mm)")

ggsave('Bodymass_FlipperLength_Species.jpg',
       width = 6, height = 6, dpi = 600)

# Species as main predictor
model_species_main <- lm(body_mass_g ~ species, data = penguins_clean)
summary(model_species_main)

check_model(model_species_main)
ggsave('Assumptions.jpg',
       width = 10, height = 8, dpi = 600)

# Species as a Covariate --------------------------------------------------

# Species as a covariate (adjustment)
model_species_cov <- lm(body_mass_g ~ flipper_length_mm + species, data = penguins_clean)
summary(model_species_cov)

# Visuals

# Predictor: “I want to know exactly how much heavier a Gentoo is than an Adelie.”
# 
# Covariate: “I care about the flipper–mass slope,
# but I’ll adjust for species so it’s fair.”
# 
# Random effect: “I don’t care which species is which.
# I just know species differ, and I’ll model that difference as a
# random offset from the global mean.”

# Visuals one vs two ------------------------------------------------------

# Clean data
penguins_clean <- penguins %>% 
  filter(!is.na(body_mass_g), !is.na(flipper_length_mm), !is.na(species))

# Mean flipper length per species (for annotations in panel 1)
mean_flipper <- penguins_clean %>%
  group_by(species) %>%
  summarise(mean_flipper = round(mean(flipper_length_mm), 1))

# --- Model 1: Species only ---
mod_species <- lm(body_mass_g ~ species, data = penguins_clean)
penguins_clean$pred_species <- predict(mod_species)

# --- Model 2: Species + flipper length (fixed effect) ---
mod_flipper_species <- lm(body_mass_g ~ flipper_length_mm + species, data = penguins_clean)
penguins_clean$pred_flipper_species <- predict(mod_flipper_species)


# Panel 1: Species only
p1 <- ggplot(penguins_clean, aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot(alpha = 0.6) +
  geom_text(data = mean_flipper, 
            aes(x = species, y = 6000,
                label = paste0("Mean flipper: ", mean_flipper, " mm")),
            inherit.aes = FALSE, vjust = -0.5,
            size = 3,fontface = "bold") +
  labs(title = "Species Only: Unadjusted Means",
       y = "Body mass (g)", x = "Species")

# Panel 2: Fixed effect adjustment
p2 <- ggplot(penguins_clean, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(alpha = 0.6) +
  geom_line(aes(y = pred_flipper_species), linewidth = 1) +
  labs(title = "Species + Flipper Length (Fixed Effect)",
       x = "Flipper length (mm)", y = "Body mass (g)")

# Combine into a single teaching figure
(p1 | p2)

ggsave('Models.jpg',
       width = 14, height = 6, dpi = 600)


# Estimated marginal means ------------------------------------------------

# Load the libraries
library(palmerpenguins)
library(emmeans)   
library(ggplot2)  
library(dplyr)

# Load the data
data(penguins)
# Let's look at the structure and handle missing values
penguins_clean <- penguins %>%
  select(species, bill_length_mm, body_mass_g) %>%
  na.omit() # Remove rows with NA values for our variables

# Look at the raw means for bill length by species
penguins_clean %>%
  group_by(species) %>%
  summarise(Raw_Mean_Bill_Length = mean(bill_length_mm))

# Fit a model: Bill Length explained by Species and Body Mass
model <- lm(bill_length_mm ~ species + body_mass_g, data = penguins_clean)

# View the model summary
summary(model)

# Calculate EMMs for each species
species_emmeans <- emmeans(model, specs = ~ species)
species_emmeans
