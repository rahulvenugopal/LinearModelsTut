# ============================================================
#  Script 05 — Programming Contest
#  Dataset : ggplot2::msleep  (mammal sleep data)
#  Packages: tidyverse, ggtext, scales
# ============================================================
#
#  Welcome to the contest!
#
#  You will use everything from Scripts 01–04:
#    ✓ Hadley's verbs   (filter, mutate, group_by, summarise, arrange)
#    ✓ Scatter plots    (geom_point, geom_smooth, facet_wrap)
#    ✓ Box plots        (geom_boxplot, geom_jitter, position_jitterdodge)
#    ✓ Styling          (theme(), element_*, scales, ggtext, custom theme)
#
#  The dataset — msleep — is already inside ggplot2/tidyverse.
#  No extra package install needed.
#
#  There are FIVE tasks below.  Each task has a data challenge and
#  a visualisation challenge.  Work through them in order.
#
#  Scoring (for self-evaluation or peer review):
#    ★ Task complete                   +2 pts each  (max 10)
#    ★★ Plot is clearly labelled        +1 pt  each  (max  5)
#    ★★★ Plot has an opinionated title  +1 pt  each  (max  5)
#    ★★★★ Bonus styling challenge       +1 pt  each  (max  5)
#  Total possible: 25 pts
#
# ============================================================

library(tidyverse)
library(ggtext)
library(scales)


# ── Meet the dataset ──────────────────────────────────────────────────────────

msleep           # print the tibble — 83 mammals, 11 variables
glimpse(msleep)
?msleep          # read the documentation!

# Key columns:
#   name          – common name of the mammal
#   genus, order  – taxonomic classification
#   vore          – diet type: carni / herbi / insecti / omni
#   conservation  – IUCN status: lc, nt, vu, en, cd, domesticated
#   sleep_total   – total daily sleep (hours)
#   sleep_rem     – REM sleep (hours)
#   sleep_cycle   – duration of a sleep cycle (hours)
#   awake         – hours awake (= 24 − sleep_total)
#   brainwt       – brain weight (kg)
#   bodywt        – body weight (kg)


# ════════════════════════════════════════════════════════════
#  TASK 1 — Cleaning & Exploration
# ════════════════════════════════════════════════════════════
#
#  Data challenge
#  ──────────────
#  1a. How many rows have missing values in sleep_rem?
#      Use summarise() and is.na().
#
#  1b. Create a clean working copy called msleep_clean by:
#      • Keeping only rows where vore is NOT missing
#      • Dropping rows where sleep_total is missing
#      • Adding a column brain_to_body = brainwt / bodywt
#      • Adding a column sleep_category:
#          "Short" if sleep_total < 8
#          "Average" if sleep_total between 8 and 14 (inclusive)
#          "Long"  if sleep_total > 14
#
#  1c. How many mammals fall into each sleep_category?
#      Use count().
#
#  Visualisation challenge
#  ────────────────────────
#  Make a bar chart (geom_col) showing the count of mammals
#  per sleep_category.  Sort bars from most to least frequent.
#  Colour the bars by sleep_category using a palette of your choice.

# ── Your code for Task 1 ──────────────────────────────────────────────────────

# 1a — count missing sleep_rem values
msleep |>
  summarise(n_missing_rem = sum(is.na(sleep_rem)))

# 1b — build msleep_clean
msleep_clean <- msleep |>
  filter(!is.na(vore), !is.na(sleep_total)) |>
  mutate(
    brain_to_body  = brainwt / bodywt,
    sleep_category = case_when(
      sleep_total < 8  ~ "Short",
      sleep_total <= 14 ~ "Average",
      TRUE             ~ "Long"
    ) |> factor(levels = c("Short", "Average", "Long"))  # ordered factor
  )

# 1c — counts
msleep_clean |> count(sleep_category)

# Visualisation — YOUR TURN
# Hint: use fct_infreq() or fct_reorder() to sort the bars




# ════════════════════════════════════════════════════════════
#  TASK 2 — Scatter Plot: Brain vs Body Weight
# ════════════════════════════════════════════════════════════
#
#  Data challenge
#  ──────────────
#  2a. Using msleep_clean, filter out rows where brainwt OR bodywt is missing.
#      Store this as msleep_weights.
#
#  2b. Calculate the log10 of both brainwt and bodywt.
#      (Without log-transforming, the chart is unreadable — try it!)
#
#  Visualisation challenge
#  ────────────────────────
#  Build a scatter plot of log10(bodywt) vs log10(brainwt),
#  coloured by vore (diet type).
#
#  Requirements:
#    • Add a linear smooth per vore group (no SE ribbon)
#    • Label at least 5 interesting mammals by name using geom_text()
#      or ggrepel::geom_text_repel() (install if desired)
#    • Meaningful axis labels ("Body weight (log₁₀ kg)" etc.)
#    • An opinionated title
#
#  Bonus ★: use scale_x_log10() / scale_y_log10() instead of
#            computing log10 manually.  How do the axis labels change?

# ── Your code for Task 2 ──────────────────────────────────────────────────────

msleep_weights <- msleep_clean |>
  filter(!is.na(brainwt), !is.na(bodywt)) |>
  mutate(
    log_bodywt  = log10(bodywt),
    log_brainwt = log10(brainwt)
  )

# Spot some interesting animals to label
msleep_weights |>
  arrange(desc(bodywt)) |>
  select(name, vore, bodywt, brainwt, log_bodywt, log_brainwt)

# Visualisation — YOUR TURN




# ════════════════════════════════════════════════════════════
#  TASK 3 — Box Plot: Sleep by Diet Type
# ════════════════════════════════════════════════════════════
#
#  Data challenge
#  ──────────────
#  3a. Using msleep_clean, calculate median and IQR of sleep_total
#      for each vore group.
#
#  3b. Reorder the vore factor by median sleep_total (descending).
#      Store as a new column vore_ordered.
#
#  Visualisation challenge
#  ────────────────────────
#  Build a box plot of sleep_total by vore (diet).
#
#  Requirements:
#    • Use the reordered factor on the x axis
#    • Overlay raw points (jittered, semi-transparent)
#    • Add median diamonds as a separate layer (from your summary in 3a)
#    • Colour by vore
#    • Apply a clean theme with no minor gridlines
#    • Opinionated title
#
#  Bonus ★: use coord_flip() to make it a horizontal box plot.
#            Does readability improve?

# ── Your code for Task 3 ──────────────────────────────────────────────────────

vore_summary <- msleep_clean |>
  summarise(
    median_sleep = median(sleep_total),
    iqr_sleep    = IQR(sleep_total),
    n            = n(),
    .by = vore
  ) |>
  arrange(desc(median_sleep))

vore_summary

msleep_clean <- msleep_clean |>
  mutate(vore_ordered = fct_reorder(vore, sleep_total, .fun = median))

# Visualisation — YOUR TURN




# ════════════════════════════════════════════════════════════
#  TASK 4 — Faceted Plot: Sleep Breakdown by Conservation Status
# ════════════════════════════════════════════════════════════
#
#  Data challenge
#  ──────────────
#  4a. Keep only mammals with a known conservation status
#      (filter out NA in conservation).
#
#  4b. Count mammals per (conservation × vore) combination.
#      Which conservation × vore combination is most common?
#
#  Visualisation challenge
#  ────────────────────────
#  Create a scatter plot of sleep_total vs awake,
#  faceted by conservation status (facet_wrap).
#
#  Requirements:
#    • colour = vore
#    • size = bodywt (gives a "bubble chart" feel)
#    • add scale_size_area() to make area ∝ bodywt
#    • meaningful labels & title
#    • strips (facet labels) should be styled with strip.text in theme()
#
#  Bonus ★: note that sleep_total + awake = 24 always.
#            Add a diagonal reference line: geom_abline(intercept = 24, slope = -1)
#            and annotate it with geom_text() or annotate().

# ── Your code for Task 4 ──────────────────────────────────────────────────────

msleep_conserved <- msleep_clean |>
  filter(!is.na(conservation))

msleep_conserved |>
  count(conservation, vore, sort = TRUE)

# Visualisation — YOUR TURN




# ════════════════════════════════════════════════════════════
#  TASK 5 — Polish: Publication-Ready Chart
# ════════════════════════════════════════════════════════════
#
#  Data challenge
#  ──────────────
#  5a. Using msleep_clean, identify the top 3 orders (taxonomic)
#      by number of species in the dataset.
#
#  5b. Filter to keep only those 3 orders.
#      Create a summary table: order, n, median_sleep, median_bodywt.
#
#  Visualisation challenge — the Big One
#  ────────────────────────────────────────
#  Build a FULLY STYLED plot comparing sleep_total across the top 3 orders.
#
#  Required elements:
#    □ Box plots (alpha = 0.3, no outlier markers)
#    □ Jittered raw points (semi-transparent, shape = 21)
#    □ Median summary overlaid as a contrasting shape
#    □ Custom colour palette (NOT the ggplot2 default)
#    □ Formatted y axis (label_number with " hrs" suffix)
#    □ ggtext in the subtitle (colour order names using HTML spans)
#    □ A custom theme (write your own or extend theme_minimal)
#    □ plot.title.position = "plot"
#    □ No minor gridlines
#    □ Opinionated title that states a finding
#    □ Caption crediting the data source
#    □ Saved with ggsave() at 300 dpi, white background
#
#  Scoring rubric for Task 5:
#    All required elements present              → 2 pts
#    Subtitle uses ggtext colour                → 1 pt (bonus)
#    Title states a genuine insight             → 1 pt (bonus)
#    Chart could appear in a data journalism piece → 1 pt (bonus, peer review)

# ── Your code for Task 5 ──────────────────────────────────────────────────────

# Step 1 — find top 3 orders
top_orders <- msleep_clean |>
  count(order, sort = TRUE) |>
  slice_head(n = 3) |>
  pull(order)

top_orders

# Step 2 — filter
msleep_top <- msleep_clean |>
  filter(order %in% top_orders)

# Step 3 — summary table
top_order_summary <- msleep_top |>
  summarise(
    n            = n(),
    median_sleep = median(sleep_total),
    median_bodywt = median(bodywt, na.rm = TRUE),
    .by = order
  )

top_order_summary

# Step 4 — YOUR TURN: build the chart




# ════════════════════════════════════════════════════════════
#  BONUS CHALLENGE — Open-Ended
# ════════════════════════════════════════════════════════════
#
#  Pick ONE question from the list below that interests you most.
#  Build a chart that answers it.  No scaffolding — fully on your own.
#
#  Questions:
#    A. Is there a relationship between sleep cycle length and
#       total sleep?  Does diet type moderate this?
#
#    B. Do herbivores or carnivores have more variable sleep patterns?
#       Visualise the distribution of sleep_total within each vore group
#       as a ridge plot (ggridges package) or violin.
#
#    C. Among endangered or vulnerable mammals, which orders sleep the most?
#       Does more sleep correlate with smaller brain-to-body ratio?
#
#  Evaluation criterion: "Does the chart clearly answer the question?"

# ── Reference: palette ideas ─────────────────────────────────────────────────

# Colourblind-safe options (great for scientific publication):
#   scale_colour_brewer(palette = "Dark2")
#   scale_colour_brewer(palette = "Set2")
#   scale_colour_manual(values = c("#E69F00","#56B4E9","#009E73","#F0E442"))

# Okabe-Ito (very colourblind friendly):
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

