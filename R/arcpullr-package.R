#' arcpullr
#'
#' A package for pulling spatial data from an ArcGIS REST API
#'
#' \if{html}{\figure{logo.png}{options: alt='logo' width='15\%'}}
#' \if{latex}{\figure{logo.png}{options: width=0.5in}}
#' The role of the arcpullr package is simple...to pull spatial data from an
#' ArcGIS REST API. These APIs are housed by various different agencies,
#' organizations, entitites, etc., but allow a consistent format for storing and
#' retrieving spatial data
#'
#' @section \code{\link{get_spatial_layer}}:
#' This function makes up the core of the package. It allows users to pull
#' spatial data given a URL of an ArcGIS REST API. There are many additional
#' query parameters that can (and probably should) be added; however, we've
#' simplified many of these out for you with the functions below.
#'
#' @section \code{\link{get_layer_by_spatial}} family of functions:
#' These functions allow you to pull layers using a spatial query. The abstract
#' syntax is wrapped into the functions, so all you have to do is pass these
#' functions an sf object of the spatial area, line, or point you want to query
#' by. These functions include get_layer_by_poly, get_layer_by_point,
#' get_layer_by_line, get_layer_by_multipoint, and get_layer_by_envelope. It
#' should be fairly obvious what type of spatial layer each function takes with
#' the exception of get_layer_by_envelope except that it isn't particularly
#' useful for a single point.
#'
#' @section \code{\link{get_image_layer}}:
#' This is one of the core functions of the package. It retrieves image service
#' layers from an ArcGIS REST API designated by the URL
#'
#' @section \code{\link{get_map_layer}}:
#' This is one of the core functions of the package. It retrieves map service
#' layers from an ArcGIS REST API designated by the URL
#'
#' @section Helper functions:
#' There are a few utility functions to help you along the way. The first is
#' \code{\link{plot_layer}}, which is a useful way to plot the spatial layer
#' you've tried to pull just to make sure it works. If you want fancier maps
#' you'd be better served with ggplot2 or tmaps, though.
#'
#' Other helpers include the \code{\link{sf_objects}} functions, which allow
#' you to easily create sf points, lines, and polygons with a few coordinates.
#'
#' Lastly, there is a \code{\link{sql_where}} function to help aid in building
#' more complex SQL WHERE clauses used to query by the where argument in the
#' retrieval functions above.
#'
#' @aliases arcpullr
#'
#' @docType package
#' @name arcpullr-package
NULL
#' NULL
