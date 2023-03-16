#' Retrieve ArcGIS REST API spatial layer by spatial query
#'
#' These functions are wrappers around \code{\link{get_spatial_layer}} that are
#' specialized for querying by a spatial layer. They will make a POST request
#' to the query URL which returns data (if available) based on the appropriate
#' spatial feature (\code{geometry}) and relationship (\code{sp_rel}).
#'
#' @param url A character string of the url for the layer to pull
#' @param geometry An \code{sf} object used for the spatial query
#' @param geom_type A character of the geometry type to be used. This param is
#' automatically specified in all \code{get_layer_by_*} functions except
#' \code{get_spatial_layer}
#' @param sp_ref The spatial reference value
#' @param sp_rel Character. The type of relationship to query by. Possible
#' options include "intersects", "contains", and "crosses"
#' @param ... Additional arguements to pass to \code{\link{get_spatial_layer}}
#'
#' @return An object of class "sf" of the appropriate layer
#' @export
#'
#' @name get_layers_by_spatial
#'
#' @examples
#' \dontrun{
#'   mke_waters <- get_layer_by_poly(wi_hydro_url, mke_county)
#' }
get_layer_by_poly <- function(url, geometry,
                              sp_rel = "contains",
                              ...) {
  if (!all(sf::st_is_valid(geometry))) {
    cat(
      "The polygon you provided is not valid. Would you like to make it valid ",
      "now?\n1: Yes\n2: No"
    )
    make_me_valid <- readline("Selection: ")
    if (tolower(make_me_valid) %in% c("1", "y", "yes")) {
      geometry <- sf::st_make_valid(geometry)
      warning(
        "Initially invalid geometries may not return the same results as ",
        "geometries that are created validly. ",
        "You may want to create your geometry correctly first."
      )
    }
  }
  return(get_layer_by_spatial(url = url,
                              geometry = format_polygon_coords(geometry),
                              geom_type = "esriGeometryPolygon",
                              sp_ref = get_sf_crs(geometry),
                              sp_rel = sp_rel_xref(sp_rel), ...))
}

#' @name get_layers_by_spatial
#' @export
get_layer_by_line <- function(url, geometry,
                              sp_rel = "intersects", ...) {
  return(get_layer_by_spatial(url = url,
                              geometry = format_line_coords(geometry),
                              geom_type = "esriGeometryPolyline",
                              sp_ref = get_sf_crs(geometry),
                              sp_rel = sp_rel_xref(sp_rel), ...))
}

#' @name get_layers_by_spatial
#' @export
get_layer_by_point <- function(url, geometry,
                               sp_rel = "intersects", ...) {
  npts <- nrow(sf::st_coordinates(geometry))
  if (npts == 1) {
    format_geom <- format_point_coords(geometry)
    geom_type <- "esriGeometryPoint"
  } else {
    format_geom <- format_multipoint_coords(geometry)
    geom_type <- "esriGeometryMultipoint"
  }
  return(get_layer_by_spatial(url = url,
                              geometry = format_geom,
                              geom_type = geom_type,
                              sp_ref = get_sf_crs(geometry),
                              sp_rel = sp_rel_xref(sp_rel), ...))
}

#' @name get_layers_by_spatial
#' @export
get_layer_by_multipoint <- function(url, geometry,
                                    sp_rel = "intersects", ...) {
  warning(
    "get_layer_by_multipoint has been generalized to get_layer_by_point.\n",
    "Please use that function instead as this one is being deprecated."
  )
  return(get_layer_by_spatial(url = url,
                              geometry = format_multipoint_coords(geometry),
                              geom_type = "esriGeometryMultipoint",
                              sp_ref = get_sf_crs(geometry),
                              sp_rel = sp_rel_xref(sp_rel), ...))
}

#' @name get_layers_by_spatial
#' @export
get_layer_by_envelope <- function(url, geometry,
                                  sp_rel = "intersects", ...) {
  return(get_layer_by_spatial(url = url,
                              geometry = format_envelope_coords(geometry),
                              geom_type = "esriGeometryEnvelope",
                              sp_ref = get_sf_crs(geometry),
                              sp_rel = sp_rel_xref(sp_rel), ...))
}

#' @name get_layers_by_spatial
#' @export
get_layer_by_spatial <- function(url, geometry, geom_type, sp_ref = NULL,
                                 sp_rel = "intersects", ...) {
  if (is.null(sp_ref)) {
    if ("sf" %in% class(geometry)) {
      sp_ref <- get_sf_crs(geometry)
    } else {
      stop("You must specify a CRS number in sp_ref ",
           "or pass an sf object for a geometry.")
    }
  }
  return(get_spatial_layer(url = url, geometry = geometry,
                           geometryType = geom_type, inSR = sp_ref,
                           spatialRel = sp_rel, ...))
}

