#' Convert RasterLayer into data.frame of colors for each pixel that can be
#' used for plotting
#'
#' This function is used internally by \code{\link{plot_layer}} to convert a
#' Raster* object to a data.frame of colors for each pixel that can be used for
#' plotting with ggplot2
#'
#' @param x A Raster* object
#'
#' @return A data.frame with 3 columns and \code{length(raster_object)} rows.
#' Two of these columns are the x-y coordinates of each pixel, and one is a
#' value for color that can be used for plotting
#'
#' @export
#'
#' @examples
#' \dontrun{
#' wi_landcover <- get_map_layer(wi_landcover_url, wis_poly)
#' wi_landcover_data <- raster_colors(wi_landcover)
#' head(wi_landcover_data)
#' }
setGeneric("raster_colors", function(x) standardGeneric("raster_colors"))

#' Convert RasterLayer into data.frame of colors that can be used for plotting
#'
#' This function is used internally by \code{\link{plot_layer}} to convert a
#' RasterLayer object to a data.frame of colors for each pixel that can be used
#' for plotting with ggplot2
#'
#' @param x A RasterLayer object
#'
#' @return A data.frame with 3 columns and \code{length(raster_object)} rows
#' @export
#'
#' @examples
#' \dontrun{
#' wi_landcover <- get_map_layer(wi_landcover_url, wis_poly)
#' wi_landcover_data <- raster_colors(wi_landcover)
#' }
setMethod("raster_colors", "RasterLayer", function(x) {
  raster_coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  raster_values <- raster::getValues(x)
  raster_cols <- raster::colortable(x)
  raster_names <- x@legend@names
  out <-
    raster_coords |>
    as.data.frame() |>
    dplyr::mutate(value = raster_values) |>
    dplyr::mutate(color = raster_cols[raster_values + 1]) |>
    dplyr::select("x", "y", "color")
  if (length(raster_names) > 0) {
    legend <- data.frame(
      color = raster_cols,
      name = raster_names
    )
    out <- dplyr::left_join(out, legend, by = "color")
  }
  return(out)
})


#' Convert RasterStack into data.frame of colors that can be used for plotting
#'
#' This function is used internally by \code{\link{plot_layer}} to convert a
#' RasterStack object to a data.frame of colors for each pixel that can be used
#' for plotting with ggplot2. Note that this function assumes that the first
#' three bands in the RasterStack objects are the RGB values and all additional
#' bands are ignored.
#'
#' @param x A RasterStack object
#'
#' @return A data.frame with 3 columns and \code{length(raster_object)} rows
#' @export
#'
#' @examples
#' \dontrun{
#' wi_leaf_off_layer <- get_image_layer(wi_leaf_off_url, wis_poly)
#' wi_leaf_off_data <- raster_colors(wi_leaf_off_layer)
#' }
setMethod("raster_colors", "RasterStack", function(x) {
  raster_coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  raster_values <- raster::getValues(x)
  raster_nbands <- length(names(x))
  raster_values <-
    raster_values <-
    raster_values |>
    as.data.frame() |>
    dplyr::select(1:3) |>
    stats::setNames(c("red", "green", "blue")) |>
    dplyr::mutate_all(tidyr::replace_na, 0L) |>
    dplyr::mutate(color = grDevices::rgb(
      .data$red, .data$green, .data$blue, maxColorValue = 255
    ))
  out <-
    raster_coords |>
    as.data.frame() |>
    cbind(raster_values) |>
    dplyr::select("x", "y", "color")
  return(out)
})

#' Convert RasterBrick into data.frame of colors that can be used for plotting
#'
#' This function is used internally by \code{\link{plot_layer}} to convert a
#' RasterBrick object to a data.frame of colors for each pixel that can be used
#' for plotting with ggplot2. Note that this function assumes that the first
#' three bands in the RasterBrick objects are the RGB values and all additional
#' bands are ignored.
#'
#' @param x A RasterBrick object
#'
#' @return A data.frame with 3 columns and \code{length(raster_object)} rows
#' @export
#'
#' @examples
#' \dontrun{
#' wi_leaf_off_layer <- get_image_layer(wi_leaf_off_url, wis_poly)
#' wi_leaf_off_data <- raster_colors(wi_leaf_off_layer)
#' }
setMethod("raster_colors", "RasterBrick", function(x) {
  raster_coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  raster_values <- raster::getValues(x)
  raster_nbands <- length(names(x))
  raster_values <-
    raster_values |>
    as.data.frame() |>
    dplyr::select(1:3) |>
    stats::setNames(c("red", "green", "blue")) |>
    dplyr::mutate_all(tidyr::replace_na, 0L) |>
    dplyr::mutate(color = grDevices::rgb(
      .data$red, .data$green, .data$blue, maxColorValue = 255
    ))
  out <-
    raster_coords |>
    as.data.frame() |>
    cbind(raster_values) |>
    dplyr::select("x", "y", "color")
  return(out)
})


#' Convert SpatRaster into data.frame of colors that can be used for plotting
#'
#' This function is used internally by \code{\link{plot_layer}} to convert a
#' SpatRaster object to a data.frame of colors for each pixel that can be used
#' for plotting with ggplot2. Note that this function assumes that the
#' SpatRaster objects use RGB values.
#'
#' @param x A SpatRaster object
#'
#' @return A data.frame with 3 columns and \code{length(raster_object)} rows
#' @export
#'
#' @examples
#' \dontrun{
#' wi_leaf_off_layer <- get_image_layer(wi_leaf_off_url, wis_poly)
#' wi_leaf_off_data <- raster_colors(wi_leaf_off_layer)
#' }
setMethod("raster_colors", "SpatRaster", function(x) {
  raster_coords <- terra::xyFromCell(x, seq_len(terra::ncell(x)))
  raster_values <- terra::values(x)
  raster_nbands <- length(names(x))
  raster_values <-
    raster_values |>
    as.data.frame() |>
    dplyr::select(1:3) |>
    stats::setNames(c("red", "green", "blue")) |>
    dplyr::mutate_all(tidyr::replace_na, 0L) |>
    dplyr::mutate(color = grDevices::rgb(
      .data$red, .data$green, .data$blue, maxColorValue = 255
    ))
  out <-
    raster_coords |>
    as.data.frame() |>
    cbind(raster_values) |>
    dplyr::select("x", "y", "color")
  return(out)
})




#' Match colors in SpatRaster coltab to the provided legend values
#'
#' Colors provided by the legend do not always correspond exactly with the
#' colors in the coltab of a SpatRaster object. They are usually pretty
#' close, though, so this function finds the closest colors, maps them to the
#' appropriate colors in the Raster* object, and applies that to the legend.
#'
#' Raster colors in \code{x} are mapped to those in \code{legend} by converting
#' the RGB hexadecimal values to a 3D vector of values for red, green and blue.
#' The closest values are then assigned using 3D Pythagorean theorem to compute
#' the distance among all colors. The minimum distance in three dimensional
#' space is the color in \code{x} that gets mapped to the appropriate color in
#' \code{legend}.
#'
#' @param legend An object of class raster_legend as returned by
#' \code{\link{get_layer_legend}}
#' @param raster_cols The colortable from a SpatRaster object. Use the first
#' item in the list
#'
#' @return A raster_legend object with corrected colors to match those in
#' \code{x}
#' @export
#'
#' @examples
#' \dontrun{
#' wi_landcover <- get_map_layer(wi_landcover_url, wis_poly)
#' legend <- get_layer_legend(wi_landcover_url)
#' new_legend <- match_legend_colors(legend, wi_landcover_url)
#' }
#'
match_legend_colors <- function(legend, raster_cols) {
  stopifnot("raster_legend" %in% class(legend))

  out <-
    legend |>
    dplyr::mutate(raster.cols = list(raster_cols)) |>
    dplyr::mutate(rgb = purrr::map2(.data$color, .data$raster.cols, \(col, rc) {
      legend_val_rgb <- grDevices::col2rgb(col)
      closest_col <-
        rc |>
        dplyr::mutate(col.dist = purrr::pmap_dbl(
          list(.data$red, .data$green, .data$blue),
          \(r, g, b) {
            sqrt(sum(c(r, g, b) - legend_val_rgb)^2)
          }
        )) |>
        dplyr::filter(.data$col.dist == min(.data$col.dist))
      closest_col
    })) |>
    dplyr::mutate(
      red = purrr::map_int(.data$rgb, \(x) x$red),
      green = purrr::map_int(.data$rgb, \(x) x$green),
      blue = purrr::map_int(.data$rgb, \(x) x$blue),
      order = purrr::map_int(.data$rgb, \(x) x$value)
    ) |>
    dplyr::select("color", "value", "order", "red", "green", "blue") |>
    # renaming value to name and order to avoid conflicting names in legend
    dplyr::rename(
      name = "value",
      value = "order"
    )

  return(out)
}
