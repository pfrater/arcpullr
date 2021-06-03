#' Returns a legend for a raster layer
#'
#' Raster layers are accompanied with legends to identify what the colors mean.
#' This function retrieves those legend values and returns them as a data.frame
#' with the associated RGB color values. This will likely be most useful for
#' plotting and analysis of map layers.
#'
#' @param url A URL to a Map or Image Service layer
#'
#' @export
#' @return A data.frame with two columns (color, values) and the number of
#' rows equal to the number of values in a layer
#'
#'
#' @examples
#' \dontrun{
#' get_layer_legend(wi_landcover_url)
#' }
get_layer_legend <- function(url) {
  if (!("magick" %in% rownames(utils::installed.packages()))) {
    stop("This function requires the magick package.\n",
         "Please install by running the following:\n",
         "    install.packages('magick')")
  }
  legend_url <- paste0(url, "/legend")
  legend_html <-
    legend_url %>%
    xml2::read_html() %>%
    rvest::html_nodes("table") %>%
    rvest::html_nodes("tr")
  legend_src <-
    legend_html %>%
    rvest::html_nodes("img") %>%
    rvest::html_attr("src")
  legend_colors <- lapply(legend_src, function(x) {
    out <-
      stringr::str_replace(x, "^data.*base64,", "") %>%
      jsonlite::base64_dec() %>%
      magick::image_read() %>%
      magick::image_data(channels = "rgb") %>%
      as.integer() %>%
      raster::brick() %>%
      raster_colors() %>%
      dplyr::count(.data$color) %>%
      dplyr::arrange(dplyr::desc(.data$n)) %>%
      dplyr::slice(1)
    return(out$color)
  }) %>% unlist()
  legend_values <-
    legend_html %>%
    rvest::html_nodes("img") %>%
    rvest::html_attr("alt")
  out <- data.frame(color = legend_colors, value = legend_values)
  return(structure(out, class = c("raster_legend", "data.frame")))
}
