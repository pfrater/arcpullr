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
#' automatically specified in \code{get_layer_by_ --poly, --line,} and
#' \code{point}
#' @param sp_ref The spatial reference value
#' @param sp_rel Character. The type of relationship to query by.
#' @param ... Additional arguements to pass to \code{\link{get_spatial_layer}}
#'
#' @return An object of class "sf" of the appropriate layer
#' @export
#'
#' @name get_layers_by_spatial
#'
#' @examples
#' base_wdnr_url <- "https://dnrmaps.wi.gov/arcgis/rest/services/"
#' hydro_path <- "WT_SWDV/WT_Inland_Water_Resources_WTM_Ext_v2/MapServer/2"
#' hydro_url <- paste0(base_wdnr_url, hydro_path)
#' mke_waters <- get_layer_by_poly(url = hydro_url, mke_county)
get_layer_by_poly <- function(url, geometry, sp_rel = "esriSpatialRelContains",
                              ...) {
  return(get_layer_by_spatial(url = url,
                              geometry = format_polygon_coords(geometry),
                              geom_type = "esriGeometryPolygon",
                              sp_rel = sp_rel, ...))
}

#' @name get_layers_by_spatial
#' @export
get_layer_by_line <- function(url, geometry,
                              sp_rel = "esriSpatialRelIntersects", ...) {
  return(get_layer_by_spatial(url = url,
                              geometry = format_line_coords(geometry),
                              geom_type = "esriGeometryPolyline",
                              sp_rel = sp_rel, ...))
}

#' @name get_layers_by_spatial
#' @export
get_layer_by_point <- function(url, geometry,
                               sp_rel = "esriSpatialRelIntersects", ...) {
  npts <- nrow(sf::st_coordinates(geometry))
  if (npts == 1) {
    format_geom <- format_point_coords(geometry)
  } else {
    format_geom <- format_multipoint_coords(geometry)
  }
  return(get_layer_by_spatial(url = url,
                              geometry = format_geom,
                              geom_type = "esriGeometryPoint",
                              sp_rel = sp_rel, ...))
}

#' @name get_layers_by_spatial
#' @export
get_layer_by_envelope <- function(url, geometry,
                                  sp_rel = "esriSpatialRelContains", ...) {
  return(get_layer_by_spatial(url = url,
                              geometry = format_envelope_coords(geometry),
                              geom_type = "esriGeometryEnvelope",
                              sp_rel = sp_rel, ...))
}

#' @name get_layers_by_spatial
#' @export
get_layer_by_spatial <- function(url, geometry, geom_type, sp_ref = "4326",
                                 sp_rel = "esriSpatialRelIntersects", ...) {
  return(get_spatial_layer(url = url, geometry = geometry,
                           geometryType = geom_type, inSR = sp_ref,
                           spatialRel = sp_rel, ...))
}

