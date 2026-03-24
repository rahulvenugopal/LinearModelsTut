# ============================================================
#  Script 04 — Styling & Polishing Plots
#  Dataset : palmerpenguins::penguins
#  Packages: tidyverse, palmerpenguins, ggtext, scales
# ============================================================
#
#  By the end of this script you will be able to:
#    • Apply and compare built-in themes
#    • Customise individual theme elements with theme()
#    • Control gridlines, axis ticks, and panel borders
#    • Format axis labels with the scales package
#    • Use ggtext for rich-text (bold, colour, HTML) in titles
#    • Build a reusable custom theme function
#    • Compose a publication-ready chart from scratch
#
#  Install once if needed:
#    install.packages(c("ggtext", "scales"))
# ============================================================

library(tidyverse)
library(palmerpenguins)
library(ggtext)   # rich text in ggplot2 labels
library(scales)   # axis formatters

penguins_clean <- penguins |> drop_na()

# Pre-compute medians — we'll reuse them across examples
species_sex_medians <- penguins_clean |>
  summarise(
    median_bill = median(bill_length_mm),
    .by = c(species, sex)
  )

# Reorder species by the spread of median bill lengths between sexes
penguins_styled <- penguins_clean |>
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) |>
  mutate(species = fct_reorder(
    species,
    bill_length_mm,
    .fun = \(x) diff(range(x))  # range within each species
  ))


# ── 0. Built-in themes — a quick tour ────────────────────────────────────────

base_plot <- ggplot(penguins_clean,
                    aes(x = species, y = bill_length_mm, fill = sex)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_point(
    aes(colour = sex), shape = 16, size = 2, alpha = 0.4,
    position = position_jitterdodge(seed = 42, jitter.width = 0.15)
  ) +
  labs(title = "Bill length by species & sex", x = NULL, y = "Bill length (mm)")

base_plot + theme_gray()       # default
base_plot + theme_bw()         # white background, black border
base_plot + theme_minimal()    # no panel border, light gridlines
base_plot + theme_classic()    # clean axes, no gridlines
base_plot + theme_light()      # light grey gridlines and border
base_plot + theme_void()       # completely empty — useful for maps

# 🔑 theme_minimal() is a good starting point for data-ink ratio reasons.
#    Edward Tufte's principle: maximise the data-ink ratio.


# ── 1. base_size — scaling everything at once ────────────────────────────────

base_plot + theme_minimal(base_size = 10)  # smaller text
base_plot + theme_minimal(base_size = 18)  # larger text — good for slides

# base_family sets the font for everything in one go
# base_plot + theme_minimal(base_size = 16, base_family = "Source Sans Pro")


# ── 2. theme() — customising individual elements ─────────────────────────────

# theme() overrides specific parts AFTER the base theme is set.

base_plot +
  theme_minimal(base_size = 15) +
  theme(
    # Title position: "plot" = flush with the left edge of the panel
    plot.title.position = "plot",

    # Caption (bottom-right text) — also flush with plot edge
    plot.caption.position = "plot",

    # Text colour — apply once to inherit everywhere
    text = element_text(colour = "grey25"),

    # Remove minor gridlines (less visual clutter)
    panel.grid.minor = element_blank(),

    # Make major gridlines very subtle
    panel.grid.major = element_line(colour = "grey92", linewidth = 0.6),

    # Remove x-axis ticks (we have a categorical axis)
    axis.ticks.x = element_blank(),

    # Move the legend to the bottom
    legend.position = "bottom"
  )

# Exercise 1 ✏️
# Add  legend.direction = "horizontal"  to the theme() call above.
# How does the legend layout change?


# ── 3. Element types in theme() ──────────────────────────────────────────────

# element_text()   — for any text element (title, axis labels, legend text …)
# element_line()   — for lines (gridlines, axis lines, ticks)
# element_rect()   — for rectangles (panel background, legend box, plot area)
# element_blank()  — completely removes an element (most powerful tool!)

base_plot +
  theme_minimal(base_size = 15) +
  theme(
    # Italicise the subtitle
    plot.subtitle = element_text(face = "italic", colour = "grey50"),

    # Bold the axis text
    axis.text     = element_text(face = "bold"),

    # Make the panel background a very pale blue
    panel.background = element_rect(fill = "#f0f4f8", colour = NA),

    # Draw a bottom border only (like The Economist style)
    axis.line.x = element_line(colour = "grey30", linewidth = 0.5),
    axis.line.y = element_blank()
  ) +
  labs(subtitle = "Chinstrap shows the widest sex difference")

# Exercise 2 ✏️
# Use element_blank() to remove:
#   • the axis text on the x axis (axis.text.x)
#   • the panel grid on the x axis (panel.grid.major.x)
# Does the plot still communicate clearly without them?


# ── 4. Formatting axes with {scales} ─────────────────────────────────────────

ggplot(penguins_clean, aes(x = species, y = body_mass_g, fill = sex)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  scale_y_continuous(
    labels = label_number(suffix = " g", big.mark = ","),  # "3,500 g"
    breaks = seq(2500, 6500, by = 500)
  ) +
  scale_fill_brewer(palette = "Set2", labels = str_to_title) +
  theme_minimal(base_size = 14) +
  labs(x = NULL, y = NULL, fill = "Sex",
       title = "Body mass distribution by species and sex")

# Other useful label_*() functions from scales:
#   label_comma()    → 1,234
#   label_percent()  → 12.3 %
#   label_dollar()   → $1,234
#   label_scientific() → 1.23e+03


# ── 5. Rich text with {ggtext} ────────────────────────────────────────────────

# ggtext lets you use HTML/Markdown in labels and titles.
# Just swap element_text() → element_markdown() in theme().

# Colour species names inline — no legend needed!
subtitle_html <-
  'Bill length of
   <span style="color:#E7872B;font-weight:bold;">Adelie</span>,
   <span style="color:#9970AB;font-weight:bold;">Chinstrap</span>, and
   <span style="color:#2D7D4F;font-weight:bold;">Gentoo</span> penguins'

ggplot(penguins_clean,
       aes(x = species, y = bill_length_mm, fill = species)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, show.legend = FALSE) +
  geom_point(
    shape = 16, size = 2, alpha = 0.4,
    position = position_jitter(width = 0.15, seed = 42),
    show.legend = FALSE
  ) +
  scale_fill_manual(values = c(
    "Adelie"    = "#E7872B",
    "Chinstrap" = "#9970AB",
    "Gentoo"    = "#2D7D4F"
  )) +
  scale_y_continuous(labels = label_number(suffix = " mm")) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title.position = "plot",
    plot.subtitle       = element_markdown(margin = margin(b = 8)),
    axis.text.x         = element_text(colour = c("#E7872B","#9970AB","#2D7D4F"),
                                       face = "bold"),
    panel.grid.minor    = element_blank(),
    panel.grid.major.x  = element_blank()
  ) +
  labs(
    title    = "Chinstrap penguins have longer bills relative to their body size",
    subtitle = subtitle_html,
    x = NULL, y = NULL,
    caption  = "Data: {palmerpenguins}"
  )

# Exercise 3 ✏️
# Wrap the title in <b>...</b> HTML tags and add font-size:18px to it
# using element_markdown(size = 18) in theme(plot.title = ...).


# ── 6. Building a reusable custom theme ───────────────────────────────────────

# Instead of repeating theme() calls, define your own theme function.

theme_penguins <- function(base_size = 14, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title.position  = "plot",
      plot.caption.position = "plot",
      text                 = element_text(colour = "grey25"),
      plot.title           = element_text(face = "bold", lineheight = 1.1),
      plot.subtitle        = element_markdown(
                               colour = "grey45",
                               margin = margin(t = 3, b = 6, unit = "mm")
                             ),
      plot.caption         = element_text(size = rel(0.65), colour = "grey55"),
      panel.grid.minor     = element_blank(),
      panel.grid.major     = element_line(colour = "grey93", linewidth = 0.6),
      axis.text            = element_text(colour = "grey40"),
      legend.position      = "bottom",
      legend.title         = element_text(face = "bold")
    )
}

# Now apply it to any plot with a single call
base_plot + theme_penguins()
base_plot + theme_penguins(base_size = 18)


# ── 7. A fully polished chart ─────────────────────────────────────────────────

caption_text <- paste0(
  "Species sorted by interquartile range of bill length.\n",
  "Data collected 2007–2009 via {palmerpenguins}."
)

final_plot <- ggplot(penguins_styled,
                     aes(x = species, y = bill_length_mm, fill = sex)) +
  # Box outlines in a neutral grey, semi-transparent fill
  geom_boxplot(
    colour        = "grey50",
    alpha         = 0.35,
    outlier.shape = NA,
    width         = 0.55
  ) +
  # Raw points jitter-dodged to align with boxes
  geom_point(
    aes(colour = sex),
    shape    = 21,
    fill     = NA,
    size     = 2.2,
    alpha    = 0.55,
    position = position_jitterdodge(jitter.width = 0.12, seed = 99)
  ) +
  # Median as large diamonds
  geom_point(
    data     = species_sex_medians,
    aes(y    = median_bill, colour = sex),
    shape    = 18,
    size     = 6,
    position = position_dodge(0.55)
  ) +
  # Colour scale — set once, shared by fill and colour
  scale_fill_manual(
    values = c(female = "#4E79A7", male = "#F28E2B"),
    labels = str_to_title
  ) +
  scale_colour_manual(
    values = c(female = "#4E79A7", male = "#F28E2B"),
    labels = str_to_title
  ) +
  scale_y_continuous(labels = label_number(suffix = " mm")) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.8, size = 4))) +
  # Remove the duplicate colour guide so legend isn't shown twice
  guides(colour = "none") +
  theme_penguins() +
  labs(
    title    = "Chinstrap penguins show the widest sex gap in bill length",
    subtitle = "◆ = median per group; raw observations shown as open circles",
    x        = NULL,
    y        = NULL,
    fill     = "Sex",
    caption  = caption_text
  )

final_plot

ggsave("04_styled_plot.png",
       plot   = final_plot,
       width  = 9,
       height = 5.5,
       dpi    = 300,
       bg     = "white")


# ── Final challenge 🏆 ────────────────────────────────────────────────────────
#
# Recreate the chart above but for body_mass_g instead of bill_length_mm.
# Then make THREE changes of your own choice to the styling:
#   (a) swap the colour palette
#   (b) change a theme element (e.g. font size, gridline colour)
#   (c) use ggtext to colour Male / Female in the subtitle in matching colours
#
# Write a sentence in the title that states something true about the data.

