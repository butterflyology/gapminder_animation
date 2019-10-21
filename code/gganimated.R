# Code for gganimate plots of gapminder data
# 2019-10-21
# Chris Hamm


# Preliminaries ----
set.seed(1138)
library("tidyverse")
library("gganimate")
library("gapminder")


# I break plots down into three sections:
# The main ggplot
# the plot that receives animations
# the thing that gets animation

# gapminder ----
data("gapminder")
glimpse(gapminder)
range(gapminder$pop)

gap_plot <- gapminder %>%
  ggplot(aes(x = log(lifeExp), y = log(gdpPercap), color = continent)) +
  geom_point(aes(size = pop)) +
  theme_minimal() +
  labs(y = "GDP PerCap", x = "Life Expectancy") +
  theme(panel.grid = element_blank(),
        panel.ontop = TRUE,
        legend.position = "none")


gap_1 <- gap_plot +
  transition_time(year) +
  ggtitle("Year: {frame_time}")

gap_1_anim <- animate(gap_1)
gap_1_anim
# anim_save(filename = "output/lgpd_lifeExp.gif", animation = gap_animated)


# Adding wake
gap_2 <- gap_plot +
  transition_time(year) +
  ggtitle("Year: {frame_time}") +
  shadow_wake(wake_length = 0.1)

gap_2_anim <- animate(gap_2)
gap_2_anim


# This shows a prevous n of time steps
gap_3 <- gap_plot +
  transition_time(year) +
  ggtitle("Year: {frame_time}") +
  shadow_trail(max_frames = 2)

gap_3_anim <- animate(gap_3)
gap_3_anim




# diamonds ----
data("diamonds")
glimpse(diamonds)

diamond_1 <- diamonds %>%
  ggplot(aes(x = carat, y = price, color = cut)) +
  geom_point(aes(size = carat), alpha = 0.5) +
  theme_minimal() +
  labs(y = "Cost", x = "Carat") +
  theme(panel.grid = element_blank(),
        panel.ontop = TRUE,
        legend.position = "none") +
  transition_states(cut) +
  ggtitle("Diamond cut: {closest_state}")

diamond_animated <- animate(diamond_1)

diamond_animated
# anim_save(filename = "output/diamonds.gif", animation = diamond_animated)
