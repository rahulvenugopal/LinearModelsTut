# Load libraries
library(tidyverse) # data wrangling + ggplot2
library(ggtext)    # enables HTML/markdown in plot text (element_markdown)

# Setup a theme
penguins_theme <- theme_minimal(base_size = 16) + # base font size for all text
  
  theme(
    plot.title.position   = 'plot', # align title to the full plot width (not just the panel)
    plot.caption.position = 'plot',                      
    
    text = element_text(color = 'grey20'),  # softer than pure black; applies to all text elements by default

    plot.title = element_text(
      lineheight = 1, # tighter line spacing for multi-line titles
      face       = 'bold', # bold makes the title stand out clearly
      size       = rel(0.8) # rel() sets size relative to base_size; 1.1 = 10% larger
    ),

    plot.subtitle = element_markdown(
      margin = margin(t = 2, b = 2, l = 2, r = 2, unit = 'mm')), # breathing room for plot

    plot.caption = element_text(
      size  = rel(0.6), # smaller than the rest of the text
      face  = 'italic', # italic to visually separate it from the main content
      color = 'grey50'), # lighter grey to de-emphasise the caption

    panel.grid.minor = element_blank(), # remove minor gridlines to reduce clutter
    panel.grid.major = element_line(
      color     = 'grey95', # very light grey so they guide the eye without dominating
      linewidth = 0.75),

    # element_markdown() on y-axis allows HTML in axis tick labels
    # (used later to render the small 'mm' unit at a different font size)
    axis.text.y = element_markdown(),

    # --- Legend ---
    legend.position = 'none',                            # hide legend; we colour-code via the subtitle instead
    legend.title    = element_text(face = 'bold'),       # bold legend title if the legend is ever re-enabled
    legend.text     = element_text(size = rel(0.9))      # slightly smaller legend item labels
  )

# Load and clean data 

penguins_raw <- palmerpenguins::penguins %>%
  drop_na() # remove rows with any missing values


# ==============================================================================
# Plot 1 — Quick exploratory plot
# ==============================================================================

# A simple jittered dot plot to get a first look at the data.
# No sorting or annotation — just a clean first pass.

penguins_raw %>%
  ggplot(aes(x = species, y = bill_length_mm, fill = sex)) +
  geom_point(
    size     = 3,
    alpha    = 0.5,                                      # semi-transparent so overlapping points are visible
    shape    = 21,                                       # shape 21 = filled circle with a separate border colour
    position = position_jitterdodge(seed = 12345)        # jitter within each dodge group; seed keeps it reproducible
  ) +
  penguins_theme +                                       # apply shared theme
  # Override legend visibility for this plot only (we still want it here)
  theme(legend.position = 'right') +

  labs(
    x     = NULL,                                        # NULL removes the axis label entirely (element_blank() only works in theme())
    y     = "Bill length",
    fill  = "Sex",
    title = "Measurements of Different Species of Penguins"
  ) 

# ==============================================================================
# Plot 2 — Sorted species by gender gap in bill length
# ==============================================================================

# Re-order species by the absolute difference in median bill length
# between males and females, so the most dimorphic species appears last.

penguins <- palmerpenguins::penguins %>%
  drop_na() %>%
  mutate(
    group_median_bill_length = median(bill_length_mm),   # compute per-group median within each species+sex combination
    .by = c(species, sex)
  ) %>%
  mutate(
    species = fct_reorder(
      species,
      group_median_bill_length,
      .fun = \(x) unique(x) %>% diff() %>% abs()        # sort factor by the absolute diff between the two group medians
    )
  )

# Caption shown at the bottom of plots 2 and 3
caption_text <- paste0(
  'Species were sorted by the difference of median bill lengths between genders\n',
  'Data was collected via the palmerpenguins R package'
)

penguins %>%
  ggplot(aes(x = species, y = bill_length_mm, fill = sex)) +
  geom_point(
    size     = 3,
    alpha    = 0.5,
    shape    = 21,
    position = position_jitterdodge(seed = 12345)
  ) +
  penguins_theme +                                       # apply shared theme
  theme(legend.position = 'right') +                    # override: show legend for this plot

  labs(
    x        = NULL,
    y        = NULL,                                     # y label removed; subtitle carries the axis description
    subtitle = 'Bill Length',
    fill     = "Sex",
    caption  = caption_text,
    title    = "Chinstrap penguins have the strongest difference in bill lengths between genders"
  )


# ==============================================================================
# Chapter 2 — Polished final plot with medians and HTML-styled subtitle
# ==============================================================================

# Pre-compute per-group medians to overlay as summary markers
median_bill_lengths <- penguins %>%
  summarise(
    median_bill_length = median(bill_length_mm),
    .by = c(species, sex)
  )


penguins %>%
  ggplot(aes(x = species, y = bill_length_mm, fill = sex)) +

  # --- Background jittered points (raw data) ---
  geom_point(
    size     = 3,
    alpha    = 0.25,                                     # more transparent than plot 1 so medians stand out
    shape    = 21,
    col      = 'white',                                  # white border blends into background; keeps focus on fill colour
    position = position_jitterdodge(seed = 12345)
  ) +

  # --- Median connector line between male and female medians ---
  geom_line(
    data      = median_bill_lengths,
    aes(y = median_bill_length, group = species),        # one line per species connecting male and female medians
    linewidth = 1,
    alpha     = 0.6
  ) +

  # --- Median dot (filled circle) ---
  geom_point(
    data     = median_bill_lengths,
    aes(y = median_bill_length),
    size     = 5,
    shape    = 21,                                       # filled circle so it inherits the fill = sex aesthetic
    position = position_dodge(0.75)                      # dodge must match the jitterdodge dodge width above
  ) +

  # --- Median tick mark (wide dash as a visual anchor) ---
  geom_point(
    data  = median_bill_lengths,
    aes(y = median_bill_length),
    size  = 10,
    alpha = 0.6,
    shape = '-'                                          # '-' shape draws a horizontal dash at the median position
  ) +

  penguins_theme +                                       # apply shared theme (legend.position = 'none' already set)

  labs(
    x        = NULL,
    y        = NULL,
    # HTML inside subtitle: colours the sex labels to match the fill scale,
    # removing the need for a legend
    subtitle = 'Bill Length of <span style="color:#F8766D;">**Male**</span> and <span style="color:#00BFC4;">**Female**</span>',
    fill     = 'Sex',
    caption  = caption_text,
    title    = "Chinstrap penguins have the strongest difference in bill lengths between genders"
  )


# Save the last rendered plot --------------------------------------------------
ggsave(
  filename = 'penguins.png',
  dpi      = 600,
  width    = 8,
  height   = 5,
  bg       = 'white'
)