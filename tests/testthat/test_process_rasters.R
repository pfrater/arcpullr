context("Process Raster functions")

nx <- 4
ny <- 4
simple_raster <- raster::raster(
  nrows = nx, ncols = ny,
  xmn = 0, ymn = 0,
  xmx = nx * ny, ymx = nx * ny
)
set.seed(1234)
raster::values(simple_raster) <- sample(1:4, nx * ny, replace = TRUE)
raster::colortable(simple_raster) <- c("black", "red", "green", "blue")

simple_raster_cols <- c(
  NA, NA, "green", "green", "red", NA, "blue", "red", "red", "green", NA, NA,
  "green", "blue", "green", "green"
)

test_that("raster_colors correctly returns RasterLayer color values", {
  cols <- raster_colors(simple_raster)$color
  expect_equal(cols, simple_raster_cols)
})

simple_legend_raster <- simple_raster
legend_names <- c(
  NA, NA, "x", "x", "y", NA, "z", "y", "y", "x", NA, NA, "x", "z", "x", "x"
)
legend_color_lookup <-
  data.frame(color = simple_raster_cols, names = legend_names) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(color)) %>%
  dplyr::arrange(color = match(color, raster::colortable(simple_raster)))
simple_legend_raster@legend@names <- c(NA, legend_color_lookup$names)

test_that("raster_colors correctly returns RasterLayer legend names", {
  col_names <- raster_colors(simple_legend_raster)$name
  expect_equal(col_names, legend_names)
})

set.seed(1234)
simple_raster1 <- simple_raster
simple_raster2 <- simple_raster
simple_raster3 <- simple_raster
raster::values(simple_raster1) <- sample(1:255, nx * ny, replace = TRUE)
raster::values(simple_raster2) <- sample(1:255, nx * ny, replace = TRUE)
raster::values(simple_raster3) <- sample(1:255, nx * ny, replace = TRUE)

raster_stack <- raster::stack(simple_raster1, simple_raster2, simple_raster3)
raster_stack_cols <- c(
  "#1C46FF", "#504FDF", "#FACE5D", "#96747A", "#650E85", "#EC7E42", "#6FB8FE",
  "#893EAF", "#8504A8", "#A6847B", "#9095D4", "#842830", "#62D46C", "#67B883",
  "#D6C357", "#5AE429"
)

test_that("raster_colors returns correct color values for RasterStack object", {
  cols <- raster_colors(raster_stack)$color
  expect_equal(cols, raster_stack_cols)
})

raster_brick <- raster::brick(simple_raster1, simple_raster2, simple_raster3)

test_that("raster_colors returns correct color values for RasterBrick object", {
  cols <- raster_colors(raster_brick)$color
  expect_equal(cols, raster_stack_cols)
})

legend <-
  data.frame(
    color = c("#1C46FF", "#EC7E42", "#5AE429"),
    value = c("z", "y", "x")
  ) %>%
  structure(class = c("raster_legend", "data.frame"))

test_that("match_raster_colors matches RasterLayer colors correctly", {
  color_match <- match_raster_colors(legend, simple_raster)
  exp_legend <- data.frame(
    color = c("blue", "red", "green"),
    value = c("z", "y", "x")
  )
  expect_equal(color_match$color, exp_legend$color)
  expect_equal(color_match$value, exp_legend$value)
})

