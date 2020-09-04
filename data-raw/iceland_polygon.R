iceland <-
  ggplot2::map_data("world", region = "iceland") %>%
  dplyr::rename(x = long, y = lat) %>%
  dplyr::select(x, y) %>%
  as.list()
iceland$range <- c(min(iceland$x), max(iceland$x),
                   min(iceland$y), max(iceland$y))
iceland$names <- "iceland"
class(iceland) <- c("map", "list")
iceland_poly <- sf::st_as_sf(iceland)

# filter to just Reykjanes
bbox <- sf_polygon(c(-25, 64.5), c(-21, 64.5), c(-21, 63),
                   c(-25, 63), c(-25, 64.5))
reykjanes <- sf::st_intersection(iceland_poly, bbox)

usethis::use_data(iceland, reykjanes, overwrite = TRUE)
