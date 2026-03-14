# Load libraries
library(tidyverse)

# Load the data and remove NAs
penguins <- palmerpenguins::penguins %>% 
  drop_na()

# Let us do a quick and dirty plot
# size, alpha, shape, fill
# position_jitterdodge
# set up the seed (show without seed)

# theming use theme_minimal
# base_size
# labels on x-axis are by default in alphabetical order
# show that the title begins from gridlines start
# Ink ratio
# label customisation - capitalise f and m of female and male using a function
# add units to y axis label using labels and scales:: suffix big.mark = ,

capitalise_legends <- \(x) str_to_title(x)

penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm, fill = sex)) + 
  geom_point(size = 3,
             alpha = 0.5,
             shape = 21,
             position = position_jitterdodge(seed = 12345)) + 
  theme_minimal(base_size = 16) + 
  
  labs(
    x = element_blank(),
    y = "Bill length",
    fill = "Sex",
    title = "Measurements of Different Species of Penguins",
  ) + 
  scale_fill_discrete(labels = capitalise_legends) + 
  scale_y_continuous(labels = scales::label_number(suffix = ' mm'))

# Sorting plot orders -----------------------------------------------------

penguins <- palmerpenguins::penguins %>% 
  drop_na() %>% 
  mutate(group_median_bill_length = median(bill_length_mm), 
         .by = c(species,sex)) %>% 
  mutate(species = fct_reorder(
    species,
    group_median_bill_length,
    .fun = \(x) unique(x) %>% diff() %>% abs()
  ))

# Adding an opinionated title
# Adding a caption
# Removing y axis lab and keeping that as subtitle
# Change default font
# Adjust the position of the title and the caption
# Theming text
# Add a margin around the subtitle
# Decrease fontsize of caption relative to rest

caption_text <- 'Species were sorted by the difference of median bill lengths between genders\nData was collected via the {palmerpenguins} R package'
  
penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm, fill = sex)) + 
  geom_point(size = 3,
             alpha = 0.5,
             shape = 21,
             position = position_jitterdodge(seed = 12345)) + 
  theme_minimal(base_size = 16,
                base_family = 'Soure Sans Pro') + 
  theme(
    plot.title.position = 'plot',
    plot.caption.position = 'plot',
    text = element_text(color = 'grey20'),
    plot.title = element_text(lineheight = 1),
    plot.subtitle = element_text(
      margin = margin(t = 4, b = 2, unit = 'mm')
    ),
    plot.caption = element_text(size = rel(0.6))
  ) + 
  
  labs(
    x = element_blank(),
    y = element_blank(),
    subtitle = 'Bill Length',
    fill = "Sex",
    caption = caption_text,
    title = "Chinstrap penguins have the strongest difference\nin bill lengths between genders",
  ) + 
  scale_fill_discrete(labels = capitalise_legends) + 
  scale_y_continuous(labels = scales::label_number(suffix = ' mm'))

# Chapter 2 ---------------------------------------------------------------

# remove minor gridlines
# make major gridlines small and less on the face
# make mm in y axis label small using ggtext
# coloring of text (hardcoding) using scales::hue
# remove the legend

# adding medians as big dots and a connector to get a sense

median_bill_lengths = penguins %>% 
  summarise(median_bill_length = median(bill_length_mm),
            .by = c(species,sex))

# custom y axis label
custom_label <- '<span style = "font-size:11px;">mm</span>'

penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm, fill = sex)) + 
  geom_point(size = 3,
             alpha = 0.25,
             shape = 21,
             col = 'white',
             position = position_jitterdodge(seed = 12345)) + 
  theme_minimal(base_size = 16,
                base_family = 'Soure Sans Pro') + 
  theme(
    plot.title.position = 'plot',
    plot.caption.position = 'plot',
    text = element_text(color = 'grey20'),
    plot.title = element_text(lineheight = 1),
    plot.subtitle =  ggtext::element_markdown(
      margin = margin(t = 4, b = 2, unit = 'mm')
    ),
    plot.caption = element_text(size = rel(0.6)),
    panel.grid.minor = element_blank(),
    axis.text.y = ggtext::element_markdown(),
    panel.grid.major = element_line(color = 'grey95', linewidth = 0.75),
    legend.position = 'none'
  ) + 
  
  labs(
    x = element_blank(),
    y = element_blank(),
    subtitle = 'Bill Length of <span style = "color:#F8766D;">**Male**</span> and <span style = "color:#00BFC4;">**Female**</span>',
    fill = 'Sex',
    caption = caption_text,
    title = "Chinstrap penguins have the strongest difference\nin bill lengths between genders",
  ) + 
  scale_fill_discrete(labels = capitalise_legends) + 
  scale_y_continuous(labels = scales::label_number(suffix = custom_label)) + 
  
  geom_point(data = median_bill_lengths,
             aes(y = median_bill_length),
             size = 5,
             shape = 21,
             position = position_dodge(0.75)) + 
  geom_line(data = median_bill_lengths,
            aes(y = median_bill_length, group = species),
            linewidth = 1,
            alpha = 0.6) + 
  geom_point(data = median_bill_lengths,
             aes(y = median_bill_length),
             size = 10,
             alpha = 0.6,
             shape = '-')

ggsave('penguins.png',
       dpi=600,
       width = 8, height = 5,
       bg = 'white')

