library(tidyverse)
library(palmerpenguins)
library(ggtext)

data <- penguins %>% 
  drop_na() %>% 
  select(species,bill_length_mm,sex)

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
       fill  = "Sex",
       title = "Measurements of Different Species of Penguins"
  ) +
  
  geom_point(
    data     = median_bill_lengths,
    aes(y = median_bill_length),
    size     = 5,
    shape    = 21,                                       # filled circle so it inherits the fill = sex aesthetic
    position = position_dodge(0.75)                      # dodge must match the jitterdodge dodge width above
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

median_bill_lengths <- data %>%
  summarise(
    median_bill_length = median(bill_length_mm),
    .by = c(species, sex)
  )
