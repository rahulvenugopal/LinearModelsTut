library(lme4)
#> Loading required package: Matrix
library(dplyr)
library(tibble)

# Convert to tibble for better printing. Convert factors to strings
sleepstudy <- sleepstudy %>% 
  as_tibble() %>% 
  mutate(Subject = as.character(Subject))

# Add two fake participants
df_sleep <- bind_rows(
  sleepstudy,
  tibble(Reaction = c(286, 288), Days = 0:1, Subject = "374"),
  tibble(Reaction = 245, Days = 0, Subject = "373"))

library(ggplot2)

xlab <- "Days of sleep deprivation"
ylab <- "Average reaction time (ms)"

ggplot(df_sleep) + 
  aes(x = Days, y = Reaction) + 
  stat_smooth(method = "lm", se = FALSE) +
  # Put the points on top of lines
  geom_point() +
  facet_wrap("Subject") +
  labs(x = xlab, y = ylab) + 
  # We also need to help the x-axis, so it doesn't 
  # create gridlines/ticks on 2.5 days
  scale_x_continuous(breaks = 0:4 * 2)
#> `geom_smooth()` using formula 'y ~ x'

df_no_pooling <- lmList(Reaction ~ Days | Subject, df_sleep) %>% 
  coef() %>% 
  # Subject IDs are stored as row-names. Make them an explicit column
  rownames_to_column("Subject") %>% 
  rename(Intercept = `(Intercept)`, Slope_Days = Days) %>% 
  add_column(Model = "No pooling") %>% 
  # Remove the participant who only had one data-point
  filter(Subject != "373")

# Fit a model on all the data pooled together
m_pooled <- lm(Reaction ~ Days, df_sleep) 

# Repeat the intercept and slope terms for each participant
df_pooled <- tibble(
  Model = "Complete pooling",
  Subject = unique(df_sleep$Subject),
  Intercept = coef(m_pooled)[1], 
  Slope_Days = coef(m_pooled)[2]
)

# Join the raw data so we can use plot the points and the lines.
df_models <- bind_rows(df_pooled, df_no_pooling) %>% 
  left_join(df_sleep, by = "Subject")

p_model_comparison <- ggplot(df_models) + 
  aes(x = Days, y = Reaction) + 
  # Set the color mapping in this layer so the points don't get a color
  geom_abline(
    aes(intercept = Intercept, slope = Slope_Days, color = Model),
    size = .75
  ) + 
  geom_point() +
  facet_wrap("Subject") +
  labs(x = xlab, y = ylab) + 
  scale_x_continuous(breaks = 0:4 * 2) + 
  # Fix the color palette 
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "top", legend.justification = "left")

p_model_comparison

m <- lmer(Reaction ~ 1 + Days + (1 + Days | Subject), df_sleep)
arm::display(m)

# Make a dataframe with the fitted effects
df_partial_pooling <- coef(m)[["Subject"]] %>% 
  rownames_to_column("Subject") %>% 
  as_tibble() %>% 
  rename(Intercept = `(Intercept)`, Slope_Days = Days) %>% 
  add_column(Model = "Partial pooling")

df_models <- bind_rows(df_pooled, df_no_pooling, df_partial_pooling) %>% 
  left_join(df_sleep, by = "Subject")

# Replace the data-set of the last plot
p_model_comparison %+% df_models

df_zoom <- df_models %>% 
  filter(Subject %in% c("335", "350", "373", "374"))

p_model_comparison %+% df_zoom

# Also visualize the point for the fixed effects
df_fixef <- tibble(
  Model = "Partial pooling (average)",
  Intercept = fixef(m)[1],
  Slope_Days = fixef(m)[2]
)

# Complete pooling / fixed effects are center of gravity in the plot
df_gravity <- df_pooled %>% 
  distinct(Model, Intercept, Slope_Days) %>% 
  bind_rows(df_fixef)
df_gravity
#> # A tibble: 2 × 3
#>   Model                     Intercept Slope_Days
#>   <chr>                         <dbl>      <dbl>
#> 1 Complete pooling               252.       10.3
#> 2 Partial pooling (average)      253.       10.5

df_pulled <- bind_rows(df_no_pooling, df_partial_pooling)

ggplot(df_pulled) + 
  aes(x = Intercept, y = Slope_Days, color = Model, shape = Model) + 
  geom_point(size = 2) + 
  geom_point(
    data = df_gravity, 
    size = 5,
    # Prevent size-5 point from showing in legend keys
    show.legend = FALSE
  ) + 
  # Draw an arrow connecting the observations between models
  geom_path(
    aes(group = Subject, color = NULL), 
    arrow = arrow(length = unit(.02, "npc")),
    show.legend = FALSE
  ) + 
  # Use ggrepel to jitter the labels away from the points
  ggrepel::geom_text_repel(
    aes(label = Subject, color = NULL), 
    data = df_no_pooling,
    show.legend = FALSE
  ) + 
  # Don't forget 373
  ggrepel::geom_text_repel(
    aes(label = Subject, color = NULL), 
    data = filter(df_partial_pooling, Subject == "373"),
    show.legend = FALSE
  ) + 
  theme(
    legend.position = "bottom", 
    legend.justification = "right"
  ) + 
  ggtitle("Pooling of regression parameters") + 
  xlab("Intercept estimate") + 
  ylab("Slope estimate") + 
  scale_shape_manual(values = c(15:18)) +
  scale_color_brewer(palette = "Dark2") 