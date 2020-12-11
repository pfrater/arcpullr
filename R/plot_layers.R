#' Plot a spatial layer
#'
#' This function plots a spatial layer as returned from
#' \code{\link{get_spatial_layer}}.
#'
#' @param sf_data An sf object as returned from \code{\link{get_spatial_layer}}
#' @param outline_poly An sf polygon to outline \code{sf_data} for context, or
#' \code{NULL}
#' @param plot_pkg Character. The plotting environment to use. Either "ggplot"
#' (default) or "base"
#'
#' @return Either a \code{ggplot} object or a plot
#' @export
#'
#' @examples
#' plot_layer(iceland)
#' plot_layer(mke_county, outline_poly = wi_poly)
#' \dontrun{
#'   g <-
#'      plot_layer(wi_counties) +
#'      ggplot2::geom_sf(data = portage_county, fill = "red") +
#'      ggplot2::geom_sf(data = mke_county, fill = "red")
#'   g
#' }
#'
plot_layer <- function(sf_data, outline_poly = NULL, plot_pkg = "ggplot") {
  if (plot_pkg == "base") {
    if (is.null(outline_poly)) {
      graphics::plot(sf_data$geom)
    } else {
      graphics::plot(outline_poly$geom)
      graphics::plot(sf_data$geom, add = TRUE)
    }
  } else if (plot_pkg %in% c("ggplot", "ggplot2")) {
    g <- ggplot2::ggplot(data = NULL)
    if (!is.null(outline_poly)) {
      g <-
        g +
        ggplot2::geom_sf(data = outline_poly, fill = ggplot2::alpha(0.01))
    }
    g <-
      g +
      ggplot2::geom_sf(data = sf_data) +
      ggplot2::theme_bw()
    return(g)
  } else {
    stop("This function only set up to work with base plotting or ggplot")
  }
}
