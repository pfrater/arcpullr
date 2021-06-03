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
    raster_coords %>%
    as.data.frame() %>%
    dplyr::mutate(value = raster_values) %>%
    dplyr::mutate(color = raster_cols[raster_values + 1]) %>%
    dplyr::select(.data$x, .data$y, .data$color)
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
    raster_values %>%
    as.data.frame() %>%
    dplyr::select(1:3) %>%
    stats::setNames(c("red", "green", "blue")) %>%
    dplyr::mutate_all(tidyr::replace_na, 0L) %>%
    dplyr::mutate(color = grDevices::rgb(
      .data$red, .data$green, .data$blue, maxColorValue = 255
    ))
  out <-
    raster_coords %>%
    as.data.frame() %>%
    cbind(raster_values) %>%
    dplyr::select(.data$x, .data$y, .data$color)
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
    raster_values %>%
    as.data.frame() %>%
    dplyr::select(1:3) %>%
    stats::setNames(c("red", "green", "blue")) %>%
    dplyr::mutate_all(tidyr::replace_na, 0L) %>%
    dplyr::mutate(color = grDevices::rgb(
      .data$red, .data$green, .data$blue, maxColorValue = 255
    ))
  out <-
    raster_coords %>%
    as.data.frame() %>%
    cbind(raster_values) %>%
    dplyr::select(.data$x, .data$y, .data$color)
  return(out)
})





#' Match colors in RasterLayer color space to the provided legend values
#'
#' Colors provided by the legend do not always correspond exactly with the
#' colors in the colortable of a RasterLayer object. They are usually pretty
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
#' @param x A RasterLayer object as returned by \code{\link{get_map_layer}}
#'
#' @return A raster_legend object with corrected colors to match those in
#' \code{x}
#' @export
#'
#' @examples
#' \dontrun{
#' wi_landcover <- get_map_layer(wi_landcover_url, wis_poly)
#' legend <- get_layer_legend(wi_landcover_url)
#' new_legend <- match_raster_colors(legend, wi_landcover_url)
#' }
match_raster_colors <- function(legend, x) {
  stopifnot("raster_legend" %in% class(legend))
  raster_cols <-
    x %>%
    raster_colors() %>%
    dplyr::pull(.data$color) %>%
    unique()
  legend_cols <- legend$color
  raster_legend_lookup <- lapply(legend_cols, function(x) {
    col_diffs <- sqrt(
      sweep(
        grDevices::col2rgb(raster_cols),
        1,
        grDevices::col2rgb(x)
      )^2
    )
    raster_col_ind <- which.min(colSums(col_diffs))
    return(data.frame(legend_col = x, raster_col = raster_cols[raster_col_ind]))
  })
  raster_legend_lookup <- do.call("rbind", raster_legend_lookup)
  corrected_legend <-
    legend %>%
    dplyr::full_join(raster_legend_lookup, by = c("color" = "legend_col")) %>%
    dplyr::select(.data$raster_col, .data$value) %>%
    dplyr::rename(color = .data$raster_col)
  return(corrected_legend)
}
