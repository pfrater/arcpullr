#' Plot a spatial layer
#'
#' This function plots a spatial layer as returned from
#' \code{\link{get_spatial_layer}}.
#'
#' @param x An sf or Raster* object as returned from a
#' \code{get_*_layer} function
#' @param outline_poly Optional. An sf polygon to outline \code{sf_data} for
#' context
#' @param plot_pkg Character. The plotting environment to use. Either "ggplot"
#' (default) or "base"
#' @param legend Logical. Include legend (TRUE, default) or not (FALSE).
#' Only applicable when plotting a Raster* object
#' @param ... Additional arguments to \code{plot_layer}
#'
#' @return Either a \code{ggplot} object, or simply plots \code{x} if
#' \code{plot_pkg = "base"}
#' @export
#'
#' @name plot_layer
#'
#' @examples
#' \dontrun{
#' plot_layer(iceland_poly)
#' plot_layer(portage_county, outline_poly = wis_poly)
#' }
setGeneric("plot_layer", function(x, ...) standardGeneric("plot_layer"))

setOldClass("sf")

#' @export
plot_layer <- function(x, ...) {
  UseMethod("plot_layer", x)
}

#' @export
plot_layer.sf <- function(x,
                         outline_poly = NULL,
                         outline_size = 1.2,
                         plot_pkg = "ggplot",
                         legend = FALSE,
                         ...) {
  if (plot_pkg == "base") {
    if (is.null(outline_poly)) {
      graphics::plot(x$geom)
    } else {
      graphics::plot(outline_poly$geom)
      graphics::plot(x$geom, add = TRUE)
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
      ggplot2::geom_sf(data = x) +
      ggplot2::theme_bw()
    return(g)
  } else {
    stop("This function only set up to work with base plotting or ggplot")
  }
}

#' Plot a RasterLayer object
#'
#' @param RasterLayer
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' wi_landcover <- get_map_layer(wi_landcover_url, wis_poly)
#' plot_layer(wi_landcover, outline_poly = wis_poly)
#' }
setMethod("plot_layer", "RasterLayer", function(x,
                                               outline_poly = NULL,
                                               outline_size = 1.2,
                                               plot_pkg = "ggplot",
                                               legend = FALSE,
                                               ...) {
  if (plot_pkg == "base") {
    if (is.null(outline_poly)) {
      raster::plot(sf_data)
    } else {
      raster::plot(sf_data)
      graphics::plot(
        outline_poly,
        add = TRUE,
        col = "transparent",
        lwd = outline_size
      )
    }
  } else if (plot_pkg %in% c("ggplot", "ggplot2")) {
    plot_data <- raster_data(x)
    plot_data <- dplyr::filter(plot_data, color != "#000000")
    g <-
      ggplot2::ggplot(data = NULL) +
      ggplot2::geom_tile(data = plot_data,
                         ggplot2::aes(x, y, fill = as.character(color)))
    if (!is.null(outline_poly)) {
      g <-
        g +
        ggplot2::geom_sf(data = outline_poly,
                         fill = ggplot2::alpha(0.01),
                         size = outline_size)
    }
    g <- g + ggplot2::scale_fill_identity()
    if (requireNamespace("cowplot", quietly = TRUE)) {
      g <- g + cowplot::theme_map()
    } else {
      g <- g + ggplot2::theme_minimal()
    }
    if (!is.null(outline_poly)) {
      g <-
        g +
        ggplot2::geom_sf(data = outline_poly,
                         fill = ggplot2::alpha(0.01),
                         size = outline_size)
    }
    if (!legend) {
      g <- g + ggplot2::theme(legend.position = "none")
    }
    return(g)
  }
})

#' Plot a RasterStack object
#'
#' @param RasterStack
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' wi_aerial <- get_map_layer(wi_leaf_off_url, wis_poly)
#' plot_layer(wi_aerial, outline_poly = wis_poly)
#' }
setMethod("plot_layer", "RasterStack", function(x,
                                               outline_poly = NULL,
                                               outline_size = 1.2,
                                               plot_pkg = "ggplot",
                                               legend = FALSE,
                                               ...) {
  if (plot_pkg == "base") {
    if (is.null(outline_poly)) {
      raster::plotRGB(x, colNA = NA)
    } else {
      raster::plotRGB(x)
      graphics::plot(
        outline_poly,
        add = TRUE,
        col = "transparent",
        lwd = outline_size
      )
    }
  } else if (plot_pkg %in% c("ggplot", "ggplot2")) {
    plot_data <- raster_data(x)
    plot_data <- dplyr::filter(plot_data, color != "#000000")
    g <-
      ggplot2::ggplot(data = NULL) +
      ggplot2::geom_tile(data = plot_data,
                         ggplot2::aes(x, y, fill = as.character(color)))
    if (!is.null(outline_poly)) {
      g <-
        g +
        ggplot2::geom_sf(data = outline_poly,
                         fill = ggplot2::alpha(0.01),
                         size = outline_size)
    }
    g <- g + ggplot2::scale_fill_identity()
    if (requireNamespace("cowplot", quietly = TRUE)) {
      g <- g + cowplot::theme_map()
    } else {
      g <- g + ggplot2::theme_minimal()
    }
    return(g)
  }
})

#' Plot an sf object
#'
#' @param sf
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' plot_layer(wis_poly)
#' }
setMethod("plot_layer", "sf", plot_layer.sf)
