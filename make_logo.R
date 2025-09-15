# Make odadata package logo

library(hexSticker)
library(UCSCXenaTools)
library(tidyverse)
set.seed(2)
dt <-
  tibble(
    x = rep(0:1, len = 50),
    y = rnorm(50),
    g = rep(letters[1:25], each = 2)
  )
p <-
  ggplot(dt) +
  aes(x, y) +
  geom_line(
    aes(group = g),
    color = "gray"
  ) +
  geom_point(
    color = "steelblue",
    size = 2
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_transparent() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )

sticker(
  p,
  package = "odadata",
  p_size = 20,
  p_fontface = "bold",
  s_x = 1,
  s_y=1,
  s_width=1.5,
  s_height = 1.5,
  p_x = 1,
  p_y = 1,
  p_color = "steelblue",
  h_fill = "white",
  h_color = "steelblue",
  url = "https://github.com/milesdwilliams15/odadata",
  filename = "inst/logo.png"
)