library(tidyverse)
library(palmerpenguins)
library(ggtext)

colnames(penguins)

data <- penguins %>% 
  drop_na() %>% 
  select(species,bill_length_mm,sex)

# Setup a theme
penguins_theme <- theme_minimal(base_size = 16) + # base font size for all text
  
  theme(
    plot.title.position   = 'plot',
    text = element_text(color = 'grey20'), 
    
    plot.title = element_text(
      lineheight = 1, # tighter line spacing for multi-line titles
      face       = 'bold'),
    
    panel.grid.minor = element_blank(), # remove minor gridlines to reduce clutter
    panel.grid.major = element_line(color     = 'grey95',
                                    linewidth = 0.75),
    
    legend.position = 'none')

median_bill_lengths <- data %>%
  summarise(
    median_bill_length = median(bill_length_mm),
    .by = c(species, sex)
  )

data %>% 
  ggplot(aes(x= species,
             y = bill_length_mm,
             fill = sex)) + 
  geom_point(size = 3,
             alpha = 0.25,
             shape = 21,
             col = "white",
             position = position_jitterdodge(seed = 12345)) + 
  
  labs(x     = NULL,
       y     = "Bill length",
       title = "Measurements of Different Species of Penguins"
  ) +
  
  geom_point(
    data     = median_bill_lengths,
    aes(y = median_bill_length),
    size     = 5,
    shape    = 21,                                       # filled circle so it inherits the fill = sex aesthetic
    position = position_dodge(0.75)                      # dodge must match the jitterdodge dodge width above
  ) +
  
  geom_line(
    data      = median_bill_lengths,
    aes(y = median_bill_length, group = species),        # one line per species connecting male and female medians
    linewidth = 1,
    alpha     = 0.6
  ) +
  
  penguins_theme + 
    theme(legend.position = 'none')

# Save the last rendered plot --------------------------------------------------
ggsave(
  filename = 'penguins_.png',
  dpi      = 600,
  width    = 8,
  height   = 5,
  bg       = 'white'
)