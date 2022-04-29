#' Plot a spatial layer
#'
#' This function plots a spatial layer as returned from
#' \code{\link{get_spatial_layer}}.
#'
#' @param x An sf or Raster* object as returned from a
#' \code{get_*_layer} function
#' @param outline_poly Optional. An sf polygon to outline \code{sf_data} for
#' context
#' @param outline_size Numeric argument that controls width of parameter
#' @param outline_color A character vector of a valid color
#' @param plot_pkg Character. The plotting environment to use. Either "ggplot"
#' (default) or "base"
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

#' @rdname plot_layer
#' @export
plot_layer.sf <- function(x,
                         outline_poly = NULL,
                         outline_size = 1.2,
                         outline_color = "gray30",
                         plot_pkg = "ggplot",
                         ...) {
  if (plot_pkg == "base") {
    if (is.null(outline_poly)) {
      graphics::plot(x$geom, ...)
    } else {
      graphics::plot(
        outline_poly$geom,
        col = "transparent",
        lwd = outline_size,
        border = outline_color,
        ...)
      graphics::plot(
        x$geom, add = TRUE,
        ...) %>% suppressWarnings()
    }
  } else if (plot_pkg %in% c("ggplot", "ggplot2")) {
    g <- ggplot2::ggplot(data = NULL)
    g <- g + ggplot2::geom_sf(data = x)
    if (!is.null(outline_poly)) {
      g <-
        g +
        ggplot2::geom_sf(data = outline_poly,
                         fill = ggplot2::alpha(0.01),
                         size = outline_size,
                         color = outline_color)
    }
    g <- g + ggplot2::theme_bw()
    return(g)
  } else {
    stop("This function only set up to work with base plotting or ggplot")
  }
}

#' Plot a RasterLayer object
#'
#' @inheritParams plot_layer
#' @param legend Logical. Only valid when plotting RasterLayers
#' retrieved from \code{\link{get_map_layer}} where legend was also retrieved
#'
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
                                               outline_color = "gray30",
                                               legend = TRUE,
                                               plot_pkg = "ggplot",
                                               ...) {
  if (plot_pkg == "base") {
    if (is.null(outline_poly)) {
      raster::plot(x, ...)
    } else {
      raster::plot(x, ...)
      graphics::plot(
        outline_poly,
        add = TRUE,
        col = "transparent",
        lwd = outline_size,
        border = outline_color,
        ...
      ) %>% suppressWarnings()
    }
  } else if (plot_pkg %in% c("ggplot", "ggplot2")) {
    plot_data <- raster_colors(x)
    plot_data <-
      plot_data %>%
      dplyr::filter(.data$color != "#000000", !is.na(.data$color))
    g <-
      ggplot2::ggplot(data = NULL) +
      ggplot2::geom_tile(data = plot_data,
                         ggplot2::aes_string("x", "y", fill = "color"))
    if (!is.null(outline_poly)) {
      g <-
        g +
        ggplot2::geom_sf(data = outline_poly,
                         fill = ggplot2::alpha(0.01),
                         size = outline_size,
                         color = outline_color)
    }
    # check to see if legend should be implemented
    if (legend && ("name" %in% names(plot_data))) {
      g <-
        g +
        ggplot2::scale_fill_identity(
          "Legend",
          guide = "legend",
          labels = plot_data$name,
          breaks = plot_data$color
        )
    } else {
      g <- g + ggplot2::scale_fill_identity()
    }
    x_crs <- raster::crs(x)
    g <- g + ggplot2::coord_sf(crs = x_crs)
    if (requireNamespace("cowplot", quietly = TRUE)) {
      g <- g + cowplot::theme_map()
    } else {
      g <- g + ggplot2::theme_minimal()
    }
    return(g)
  } else {
    stop("This function only set up to work with base plotting or ggplot")
  }
})

#' Plot a RasterStack object
#'
#' @inheritParams plot_layer
#'
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
                                               outline_color = "gray30",
                                               plot_pkg = "ggplot",
                                               ...) {
  if (plot_pkg == "base") {
    if (is.null(outline_poly)) {
      raster::plotRGB(x, colNA = NA, ...)
    } else {
      raster::plotRGB(x, ...)
      graphics::plot(
        outline_poly,
        add = TRUE,
        col = "transparent",
        lwd = outline_size,
        border = outline_color,
        ...
      ) %>% suppressWarnings()
    }
  } else if (plot_pkg %in% c("ggplot", "ggplot2")) {
    plot_data <- raster_colors(x)
    plot_data <- dplyr::filter(plot_data, .data$color != "#000000")
    g <-
      ggplot2::ggplot(data = NULL) +
      ggplot2::geom_tile(data = plot_data,
                         ggplot2::aes_string("x", "y",
                                             fill = as.character("color")))
    if (!is.null(outline_poly)) {
      g <-
        g +
        ggplot2::geom_sf(data = outline_poly,
                         fill = ggplot2::alpha(0.01),
                         size = outline_size,
                         color = outline_color)
    }
    x_crs <- raster::crs(x)
    g <- g + ggplot2::scale_fill_identity() + ggplot2::coord_sf(crs = x_crs)
    if (requireNamespace("cowplot", quietly = TRUE)) {
      g <- g + cowplot::theme_map()
    } else {
      g <- g + ggplot2::theme_minimal()
    }
    return(g)
  } else {
    stop("This function only set up to work with base plotting or ggplot")
  }
})


#' Plot a RasterBrick object
#'
#' @inheritParams plot_layer
#'
#' @export
#'
#' @examples
#' \dontrun{
#' wi_aerial <- get_map_layer(wi_leaf_off_url, wis_poly)
#' plot_layer(wi_aerial, outline_poly = wis_poly)
#' }
setMethod("plot_layer", "RasterBrick", function(x,
                                                outline_poly = NULL,
                                                outline_size = 1.2,
                                                outline_color = "gray30",
                                                plot_pkg = "ggplot",
                                                ...) {
  if (plot_pkg == "base") {
    if (is.null(outline_poly)) {
      raster::plotRGB(x, colNA = NA, ...)
    } else {
      raster::plotRGB(x, ...)
      graphics::plot(
        outline_poly,
        add = TRUE,
        col = "transparent",
        lwd = outline_size,
        border = outline_color,
        ...
      ) %>% suppressWarnings()
    }
  } else if (plot_pkg %in% c("ggplot", "ggplot2")) {
    plot_data <- raster_colors(x)
    plot_data <- dplyr::filter(plot_data, .data$color != "#000000")
    g <-
      ggplot2::ggplot(data = NULL) +
      ggplot2::geom_tile(data = plot_data,
                         ggplot2::aes_string("x", "y",
                                             fill = as.character("color")))
    if (!is.null(outline_poly)) {
      g <-
        g +
        ggplot2::geom_sf(data = outline_poly,
                         fill = ggplot2::alpha(0.01),
                         size = outline_size,
                         color = outline_color)
    }
    x_crs <- raster::crs(x)
    g <- g + ggplot2::scale_fill_identity() + ggplot2::coord_sf(crs = x_crs)
    if (requireNamespace("cowplot", quietly = TRUE)) {
      g <- g + cowplot::theme_map()
    } else {
      g <- g + ggplot2::theme_minimal()
    }
    return(g)
  } else {
    stop("This function only set up to work with base plotting or ggplot")
  }
})



#' Plot an sf object
#'
#' @inheritParams plot_layer
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot_layer(wis_poly)
#' }
setMethod("plot_layer", "sf", plot_layer.sf)
