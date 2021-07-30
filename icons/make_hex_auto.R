library(hexSticker)
library(tidyverse)
library(magick)

globe <-
  "icons/globe.png" %>%
  image_read() %>%
  image_scale("200")
cloud <-
  "icons/white_cloud.png" %>%
  image_read() %>%
  image_scale("200")
pin <-
  "icons/pin.png" %>%
  image_read() %>%
  image_scale("200")

cloudray1 <- arcpullr::sf_line(c(-88.75, 48.4), c(-87, 50))
cloudray2 <- arcpullr::sf_line(c(-87.2, 47.95), c(-85.3, 50))
cloudray3 <- arcpullr::sf_line(c(-85.9, 47.1), c(-83.75, 50))

plot <-
  ggplot() +
  geom_sf(data = cloudray1, size = 1, color = "#FFB612", lineend = "round") +
  geom_sf(data = cloudray2, size = 1, color = "#FFB612", lineend = "round") +
  geom_sf(data = cloudray3, size = 1, color = "#FFB612", lineend = "round") +
  coord_sf(xlim = c(-93, -82), ylim = c(42, 53)) +
  annotation_raster(globe, ymin = 42, ymax = 49, xmin = -94.5, xmax = -84.5) +
  annotation_raster(cloud, ymin = 49, ymax = Inf, xmin = -89, xmax = Inf) +
  annotation_raster(pin, ymin = 46.55, ymax = 48.3, xmin = -88, xmax = -90.5) +
  theme_void()

sticker(
  plot,
  s_x = 1,
  s_width = 3,
  s_height = 1.25,
  p_y = 1.45,
  p_size = 20,
  p_family = "sans",
  p_fontface = "bold",
  p_color = "black",
  h_fill="#72a3d4", #fill hex code
  h_color="#2382a1", #border hex code
  package = "arcpullr",
  filename = "icons/logo.png"
)

file.copy("icons/logo.png", "man/figures/logo.png", overwrite = TRUE)
