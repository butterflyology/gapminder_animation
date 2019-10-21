# Code for gganimate plots of gapminder data
# 2019-10-21
# Chris Hamm


# Preliminaries ----
set.seed(1138)
library("tidyverse")
library("gganimate")
library("gapminder")


# gapminder ----
data("gapminder")
glimpse(gapminder)
range(gapminder$pop)

gap_1 <- gapminder %>%
  ggplot(aes(x = log(lifeExp), y = log(gdpPercap), color = continent)) +
  geom_point(aes(size = pop)) +
  theme_minimal() +
  labs(y = "GDP PerCap", x = "Life Expectancy") +
  theme(panel.grid = element_blank(),
        panel.ontop = TRUE,
        legend.position = "none")
gap_1

gap_anim <- gap_1 +
  transition_time(year) +
  ggtitle("Year: {frame_time}")

gap_animated <- animate(gap_anim)

gap_animated
# anim_save(filename = "output/lgpd_lifeExp.gif", animation = gap_animated)


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
