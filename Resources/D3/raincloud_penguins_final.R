# Raincloud Plot - Cedric Scherer Style
# Palmer Penguins bill ratio visualization

# Load packages ----
library(tidyverse)
library(colorspace)
library(ggdist)

# Define color palette ----
pal <- c("#FF8C00", "#A034F0", "#159090")

# Load data ----
df_penguins <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv') %>% 
  mutate(species = if_else(species == "Adelie", "Adélie", species))

# Prepare data ----
df_peng_iqr <- 
  df_penguins %>% 
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) %>% 
  filter(!is.na(bill_ratio)) %>% 
  group_by(species) %>% 
  mutate(
    median = median(bill_ratio),
    q25 = quantile(bill_ratio, probs = .25),
    q75 = quantile(bill_ratio, probs = .75),
    n = n()
  ) %>% 
  ungroup() %>% 
  mutate(species_num = as.numeric(fct_rev(species)))

# Summary data ----
df_summary_median <- 
  df_peng_iqr %>% 
  distinct(species, species_num, median)

df_summary_n <- 
  df_peng_iqr %>% 
  group_by(species, species_num) %>% 
  summarize(n = first(n), max = max(bill_ratio), .groups = "drop")

# Background rectangles ----
df_rect <- tibble(
  xmin = c(-Inf, 2.46, 3.27),
  xmax = c(Inf, Inf, Inf),
  ymin = c(3, 2, 1),
  ymax = c(Inf, Inf, Inf)
)

# Create plot ----
rain <- 
  ggplot(df_peng_iqr, aes(x = bill_ratio, y = species_num - .2, color = species)) +
  
  # Background
  geom_rect(
    data = df_rect,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "white"
  ) +
  
  # Dotted line to median
  geom_linerange(
    data = df_summary_median,
    aes(xmin = -Inf, xmax = median, y = species_num, color = species),
    inherit.aes = FALSE,
    linetype = "dotted",
    size = .7
  ) +
  
  # Box plot whiskers
  geom_boxplot(
    aes(group = species_num, 
        group = species,
        color = after_scale(darken(color, .1, space = "HLS"))),
    width = 0,
    size = .9
  ) +
  
  # IQR rectangles
  geom_rect(
    aes(xmin = q25, xmax = median,
        ymin = species_num - .05, ymax = species_num - .35),
    inherit.aes = FALSE,
    fill = "grey89"
  ) +
  geom_rect(
    aes(xmin = q75, xmax = median,
        ymin = species_num - .05, ymax = species_num - .35),
    inherit.aes = FALSE,
    fill = "grey79"
  ) +
  
  # Q1 and Q3 lines
  geom_segment(
    aes(x = q25, xend = q25,
        y = species_num - .05, yend = species_num - .35,
        color = after_scale(darken(color, .05, space = "HLS"))),
    size = .25
  ) +
  geom_segment(
    aes(x = q75, xend = q75,
        y = species_num - .05, yend = species_num - .35,
        color = after_scale(darken(color, .05, space = "HLS"))),
    size = .25
  ) +
  
  # Individual points (barcode)
  geom_point(
    shape = "|",
    size = 5,
    alpha = .33
  ) +
  
  # Density plot
  stat_halfeye(
    aes(y = species_num, fill = after_scale(lighten(color, .5))),
    shape = 18,
    point_size = 3,
    interval_size = 1.8,
    adjust = .5,
    .width = c(0, 1)
  ) +
  
  # Median labels
  geom_text(
    data = df_summary_median,
    aes(x = median, y = species_num + .12,
        label = format(round(median, 2), nsmall = 2)),
    inherit.aes = FALSE,
    color = "white",
    fontface = "bold",
    size = 3.5
  ) +
  
  # Sample size labels
  geom_text(
    data = df_summary_n,
    aes(x = max + .01, y = species_num + .02,
        label = glue::glue("n = {n}"),
        color = species),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 3.5,
    hjust = 0
  ) +
  
  # Scales
  coord_cartesian(clip = "off") +
  scale_x_continuous(
    limits = c(1.57, 3.7),
    breaks = seq(1.6, 3.6, by = .2),
    expand = c(.001, .001)
  ) +
  scale_y_continuous(
    limits = c(.55, NA),
    breaks = 1:3,
    labels = c("Gentoo", "Chinstrap", "Adélie"),
    expand = c(0, 0)
  ) +
  scale_color_manual(values = pal, guide = "none") +
  scale_fill_manual(values = pal, guide = "none") +
  
  # Labels & theme
  labs(
    x = "Bill ratio",
    y = NULL,
    subtitle = "Distribution of Bill Ratio Across Penguin Species"
  ) +
  theme_minimal(base_size = 15, base_family = "Inter") +
  theme(
    panel.grid.major = element_line(color = "grey92", size = .4),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(color = "grey30", margin = margin(t = 7)),
    axis.text = element_text(color = "grey50"),
    axis.text.y = element_text(color = rev(pal), size = 13, face = "bold"),
    axis.ticks = element_line(color = "grey92", size = .4),
    axis.ticks.length.y = unit(0, "lines"),
    plot.subtitle = element_text(hjust = 0, color = "grey30", size = 12),
    plot.margin = margin(t = 5, r = 15, b = 5, l = 15)
  )

rain

# Save ----
ggsave(
  "raincloud_penguins.png",
  width = 10,
  height = 6,
  dpi = 600,
  bg = "white"
)