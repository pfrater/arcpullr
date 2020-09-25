#' Convert coordinates from an 'sf' object to formatted well-known text
#'
#' Use this function to convert the coordinates of a sf polygon object to
#' a string of well known text. The output can be passed to an ArcGIS REST
#' API to perform a spatial query.
#'
#' Spatial queries from an ArcGIS REST API require specific text inputs
#' formatted in a way called well-known text (WKT). ArcGIS REST APIs have their
#' own syntax for how the text is taken. These functions will format sf objects
#' in the correct way to be able to make spatial queries from a ArcGIS REST
#' API
#'
#' @param sf_obj An sf object
#' @param crs The coordinate reference system. Defaults to 4326
#'
#' @return String of well known text
#' @export
#' @name format_coords
#'
#' @examples
#' mke_polygon_coords <- format_polygon_coords(mke_county)
format_polygon_coords <- function(sf_obj, crs = 4326) {
  return(format_coords(sf_obj, geom_type = "polygon", crs = crs))
}

#' @rdname format_coords
#' @export
format_line_coords <- function(sf_obj, crs = 4326) {
  return(format_coords(sf_obj, geom_type = "polyline", crs = crs))
}

#' @rdname format_coords
#' @export
format_multipoint_coords <- function(sf_obj, crs = 4326) {
  return(format_coords(sf_obj, geom_type = "multipoint", crs = crs))
}

#' @rdname format_coords
#' @export
format_point_coords <- function(sf_obj, crs = 4326) {
  out <-
    sf_obj %>%
    sf::st_transform(crs = crs) %>%
    sf::st_coordinates() %>%
    data.frame() %>%
    dplyr::select(X,Y)
}

#' @rdname format_coords
#' @export
format_envelope_coords <- function(sf_obj, crs = 4326) {
  bbox <- sf::st_bbox(sf_obj)
  coord_string <- paste(names(bbox), bbox, sep = " : ", collapse = ", ")
  return(coord_string)
}

#' @rdname format_coords
#' @export
format_coords <- function(sf_obj, geom_type, crs = 4326) {
  geometry_type <- switch(
    geom_type,
    multipoint = "points",
    polyline = "paths",
    polygon = "rings"
  )
  geometry_type <- sprintf("'%s'", geometry_type)
  out <-
    sf_obj %>%
    sf::st_transform(crs = crs) %>%
    sf::st_coordinates() %>%
    data.frame() %>%
    dplyr::select(X,Y) %>%
    tidyr::unite(col= "coordinates",sep = ",") %>%
    dplyr::mutate(coordinates = paste("[",coordinates,"]",sep="")) %>%
    dplyr::summarise(coordinates =  paste(coordinates,collapse = ",")) %>%
    dplyr::mutate(coordinates = paste("{", geometry_type, ":[[",
                                      coordinates,
                                      "]],'spatialReference':{'wkid':", crs,
                                      "}}", sep = "")) %>%
    dplyr::pull()
  return(out)
}


#' Create sf objects from coordinates
#'
#' These are simple wrapper functions for creating sf objects from points
#'
#' @param ... The coordinates of the object
#' @param crs The coordinate reference system. Defaults to 4326
#'
#' @return
#' @export
#' @name sf_objects
#'
#' @examples
#' pt_a <- c(-90, 45)
#' pt_b <- c(-89, 44)
#' sf_pt <- sf_points(pt_a)
#' sf_lines <- sf_lines(pt_a, pt_b)
sf_lines <- function(..., crs = 4326) {
  coords <- do.call("rbind", list(...))
  ls <- sf::st_linestring(coords)
  sfc <- sf::st_sfc(ls, crs = crs)
  sf <- sf::st_sf(geom = sfc)
  return(sf)
}

#' @rdname sf_objects
#' @export
sf_points <- function(..., crs = 4326) {
  coords <- do.call("rbind", list(...))
  ls <- sf::st_multipoint(coords)
  sfc <- sf::st_sfc(ls, crs = crs)
  sf <- sf::st_sf(geom = sfc)
  return(sf)
}

#' @rdname sf_objects
#' @export
sf_polygon <- function(..., crs = 4326) {
  ls <- sf::st_polygon(list(rbind(...)))
  sfc <- sf::st_sfc(ls, crs = crs)
  sf <- sf::st_sf(geom = sfc)
  return(sf)
}



#' Format a SQL where clause from arguments
#'
#' This function will create a where statement that is compatible with
#' \code{\link{get_spatial_layer}} and other associated functions
#' (i.e. \code{\link{get_hydro_layer}}, etc.). This statement can then be passed
#' to the \code{where} argument in any of these functions.
#'
#' @param ... Named objects to be queried by
#' @param rel_op Character. The relational operator in the SQL clause (i.e. "=",
#' "IN", "NOT IN", etc.). If a single rel_op is provide with multiple ...
#' parameters then it will be recycled \code{length(...)} times.
#'
#' @return A character string that can be passed to the where argument of
#' \code{get_spatial_layer}
#' @export
#'
#' @examples
#' wbics <- sql_where(WATERBODY_WBIC = c(805400, 804600), rel_op = "IN")
#' base_wdnr_url <- "https://dnrmaps.wi.gov/arcgis/rest/services/"
#' hydro_path <- "WT_SWDV/WT_Inland_Water_Resources_WTM_Ext_v2/MapServer/3"
#' hydro_url <- paste0(base_wdnr_url, hydro_path)
#' lakes <- get_spatial_layer(url = hydro_url, where = wbics)
#' plot_layer(lakes)
sql_where <- function(..., rel_op = "=") {
  args <- list(...)
  if (is.null(names(args)) | any(names(args) == "")) {
    stop("All ... parameters must be named arguments.")
  }
  if (length(args) > 1) {
    if (length(rel_op) == 1) {
      rel_op <- rep(rel_op, times = length(args))
    } else if (length(args) != length(rel_op)) {
      stop("If multiple ... parameters are entered, then a vector of rel_ops ",
           "the length of the number of parameters must be passed.")
    }
  }
  out_list <- lapply(1:length(args), function(x) {
    if (length(args[[x]]) > 1) {
      if (is.character(args[[x]])) {
        x_vals <- paste0("( '", paste(args[[x]], collapse = "' , '"), "' )")
      } else {
        x_vals <- paste0("(", paste(args[[x]], collapse = ", "), ")")
      }
    } else {
      if (is.character(args[[x]])) {
        x_vals <- paste0("'", args[[x]], "'")
      } else {
        x_vals <- args[[x]]
      }
    }
    return(paste(names(args)[x], rel_op[x], x_vals))
  })
  if (length(out_list) > 1) {
    out <- paste(out_list, collapse = " AND ")
  } else {
    out <- out_list[[1]]
  }
  return(out)
}


