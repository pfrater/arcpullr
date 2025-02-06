# context("Process Raster functions")

nx <- 4
ny <- 4
simple_raster <- terra::rast(
  nrow = nx, ncol = ny,
  xmin = 0, ymin = 0,
  xmax = nx * ny, ymax = nx * ny
)
set.seed(1234)
terra::values(simple_raster) <- sample(1:3, nx * ny, replace = TRUE)
col1 <- "#EC7E42"
col2 <- "#5AE429"
col3 <- "#1C46FF"
# col1 <- "tomato"
# col2 <- "limegreen"
# col3 <- "royalblue4"
terra::coltab(simple_raster) <- data.frame(
  value = 1:3,
  col = c(col1, col2, col3)
)

simple_raster_cols <- c(
  NA, NA, col2, col2, col1, NA, col3, col1, col1, col2, NA, NA,
  col2, col3, col2, col2
)

simple_legend_raster <- simple_raster
legend_names <- c(
  NA, NA, "x", "x", "y", NA, "z", "y", "y", "x", NA, NA, "x", "z", "x", "x"
)
legend_color_lookup <-
  data.frame(color = simple_raster_cols, names = legend_names) |>
  dplyr::distinct() |>
  dplyr::filter(!is.na(color)) |>
  dplyr::arrange(color = match(color, terra::coltab(simple_raster)))

legend <-
  data.frame(
    color = c("#1C46FF", "#EC7E42", "#5AE429"),
    value = c("z", "y", "x")
  ) |>
  structure(class = c("raster_legend", "data.frame"))

test_that("match_legend_colors matches SpatRaster colors correctly", {
  simple_rast_cols <-
    terra::coltab(simple_raster)[[1]] |>
    dplyr::rename(value = values)
  color_match <- match_legend_colors(legend, simple_rast_cols)
  exp_legend <- data.frame(
    color =  c("#1C46FF", "#EC7E42", "#5AE429"),
    name = c("z", "y", "x"),
    value = c(3, 1, 2),
    red = c(28, 236, 90),
    green = c(70, 126, 228),
    blue = c(255, 66, 41)
  )
  expect_equal(color_match$color, exp_legend$color)
  expect_equal(color_match$value, exp_legend$value)
})

