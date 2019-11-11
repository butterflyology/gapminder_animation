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
  ggplot(aes(y = lifeExp, x = gdpPercap, color = continent, size = pop)) +
  geom_point() +
  theme_minimal() +
  labs(y = "GDP PerCap", x = "Life Expectancy") +
  scale_y_continuous(breaks = seq(45, 85, 5), labels = as.character(seq(45, 85, 5))) +
  scale_x_continuous(breaks = c(1000, 2000, 4000, 8000, 16000, 32000, 64000, 128000), labels = c("1000", "2000", "4000", "8000", "16K", "32K", "64K", "128K")) +
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


# more gapminder ----
p <- gapminder %>% 
  ggplot(
    aes(x = gdpPercap, y = lifeExp, size = pop, color = country)) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectency")

p + transition_time(year) +
  labs(title = "Year: {frame_time}")

p + facet_wrap(~continent) +
  transition_time(year) + 
  labs(title = "Year: {frame_time}")

p + transition_time(year) +
  labs(title = "Year: {frame_time}") +
  view_follow(fixed_y = TRUE)

p + transition_time(year) +
  labs(title = "Year: {frame_time}") +
  shadow_wake(wake_length = 0.1, alpha = TRUE)


# airquality ----
data(airquality)
glimpse(airquality)
range(airquality$Month)

t <- airquality %>% 
  ggplot(
    aes(x = Day, y = Temp, group = Month, color = factor(Month))
  ) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Day of Month", y = "Temperature") +
  theme(legend.position = "top")
t

t + transition_reveal(Day)

t + 
  geom_point() +
  transition_reveal(Day)


t2 <- airquality %>% 
  ggplot(
    aes(x = Day, y = Temp, group = Month, color = factor(Month))
  ) +
  geom_point() +
  scale_color_viridis_d() +
  labs(x = "Day of Month", y = "Temperature") +
  theme(legend.position = "top")
t

t + transition_reveal(Day) +
  shadow_wake(wake_length = 1, alpha = TRUE, size = TRUE)

t + geom_point(aes(group = seq_along(Day))) +
  transition_reveal(Day)

mean_temp <- airquality %>% 
  group_by(Month) %>% 
  summarize(mTemp = mean(Temp))
mean_temp

t3 <- mean_temp %>% 
  ggplot(
    aes(x = Month, y = mTemp, fill = mTemp)) +
  geom_col() +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    panel.ontop = TRUE)

t3

t3 + transition_states(Month, wrap = FALSE)

t3 + transition_states(Month, wrap = FALSE) +
  shadow_mark() +
  enter_grow() + 
  enter_fade()

# anim_save()


# boxplots ----
range(mtcars$gear)

mtcars %>% 
  ggplot(aes(x = factor(cyl), y = mpg)) +
  geom_boxplot() +
  transition_states(
    gear, 
    transition_length = 2,
    state_length = 1) +
  enter_fade() +
  exit_shrink() + 
  ease_aes('sine-in-out') +
  labs(title = "Gears: {closest_state}")
