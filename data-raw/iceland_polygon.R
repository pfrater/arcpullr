iceland_poly <-
  ggplot2::map_data("world", region = "iceland") %>%
  dplyr::rename(x = long, y = lat) %>%
  dplyr::select(x, y) %>%
  as.list()
iceland_poly$range <- c(min(iceland_poly$x), max(iceland_poly$x),
                   min(iceland_poly$y), max(iceland_poly$y))
iceland_poly$names <- "iceland"
class(iceland_poly) <- c("map", "list")
iceland_poly <- sf::st_as_sf(iceland_poly)

# filter to just Reykjanes
bbox <- sf_polygon(c(-25, 64.5), c(-21, 64.5), c(-21, 63),
                   c(-25, 63), c(-25, 64.5))
reykjanes_poly <- sf::st_intersection(iceland_poly, bbox)

usethis::use_data(iceland_poly, reykjanes_poly, overwrite = TRUE)
