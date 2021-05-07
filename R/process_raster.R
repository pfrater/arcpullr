#' Convert RasterLayer into data.frame that can be used for plotting
#'
#' This function is used internally by \code{\link{plot_layer}} to convert a
#' Raster* object to a data.frame that can be used for plotting with ggplot2
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
#' wi_landcover_data <- raster_data(wi_landcover)
#' head(wi_landcover_data)
#' }
setGeneric("raster_data", function(x) standardGeneric("raster_data"))

#' Convert RasterLayer into data.frame that can be used for plotting
#'
#' This function is used internally by \code{\link{plot_layer}} to convert a
#' RasterLayer object to a data.frame that can be used for plotting with ggplot2
#'
#' @param x A RasterLayer object
#'
#' @return A data.frame with 3 columns and \code{length(raster_object)} rows
#' @export
#'
#' @examples
#' \dontrun{
#' wi_landcover <- get_map_layer(wi_landcover_url, wis_poly)
#' wi_landcover_data <- raster_data(wi_landcover)
#' }
setMethod("raster_data", "RasterLayer", function(x) {
  max_val <- raster::maxValue(x)
  raster_coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  raster_values <- raster::getValues(x)
  raster_colors <- raster::colortable(x)
  out <-
    raster_coords %>%
    as.data.frame() %>%
    dplyr::mutate(value = raster_values) %>%
    dplyr::mutate(color = raster_colors[raster_values + 1]) %>%
    dplyr::filter(value != max_val) %>%
    dplyr::select(x, y, color)
  return(out)
})


#' Convert RasterStack into data.frame that can be used for plotting
#'
#' This function is used internally by \code{\link{plot_layer}} to convert a
#' RasterLayer object to a data.frame that can be used for plotting with ggplot2
#'
#' @param x A RasterStack object
#'
#' @return A data.frame with 3 columns and \code{length(raster_object)} rows
#' @export
#'
#' @examples
#' \dontrun{
#' wi_leaf_off_layer <- get_image_layer(wi_leaf_off_url, wis_poly)
#' wi_leaf_off_data <- raster_data(wi_leaf_off_layer)
#' }
setMethod("raster_data", "RasterStack", function(x) {
  raster_coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  raster_values <- raster::getValues(x)
  raster_values <-
    raster_values %>%
    as.data.frame() %>%
    setNames(c("red", "green", "blue", "extra")) %>%
    dplyr::mutate(color = grDevices::rgb(red, green, blue, maxColorValue = 255))
  out <-
    raster_coords %>%
    as.data.frame() %>%
    cbind(raster_values) %>%
    dplyr::select(x, y, color)
  return(out)
})
