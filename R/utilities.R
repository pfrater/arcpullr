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
#'
#' @return String of well known text
#' @export
#' @name format_coords
#'
#' @examples
#' mke_polygon_coords <- format_polygon_coords(mke_county)
format_polygon_coords <- function(sf_obj) {
  return(format_coords(sf_obj, geom_type = "polygon"))
}

#' @rdname format_coords
#' @export
format_line_coords <- function(sf_obj) {
  return(format_coords(sf_obj, geom_type = "polyline"))
}

#' @rdname format_coords
#' @export
format_multipoint_coords <- function(sf_obj) {
  return(format_coords(sf_obj, geom_type = "multipoint"))
}

#' @rdname format_coords
#' @export
format_point_coords <- function(sf_obj) {
  crs <- get_sf_crs(sf_obj)
  out <-
    sf_obj %>%
    sf::st_coordinates() %>%
    data.frame() %>%
    dplyr::select(.data$X, .data$Y)
  pt_str <- paste(paste0("'", tolower(names(out)), "':"), out, collapse = ",")
  coord_string <- paste("{", pt_str, ", {'spatialReference': ", crs, "}}")
  return(coord_string)
}

#' @rdname format_coords
#' @export
format_envelope_coords <- function(sf_obj) {
  bbox <- sf::st_bbox(sf_obj)
  coord_string <- paste(names(bbox), bbox, sep = " : ", collapse = ", ")
  return(coord_string)
}

#' @rdname format_coords
#' @param geom_type Either "points", "paths", or "rings". Choose wisely
#' @export
# format_coords <- function(sf_obj, geom_type) {
#   geometry_type <- switch(
#     geom_type,
#     multipoint = "points",
#     polyline = "paths",
#     polygon = "rings"
#   )
#   left_bracket <- switch(
#     geom_type,
#     multipoint = "[",
#     polyline = "[[",
#     polygon = "[[")
#   right_bracket <- switch(
#     geom_type,
#     multipoint = "",
#     polyline = "]]",
#     polygon = "]]")
#
#   geometry_type <- sprintf("'%s'", geometry_type)
#   crs <- get_sf_crs(sf_obj)
#   out <-
#     sf_obj %>%
#     sf::st_coordinates() %>%
#     data.frame() %>%
#     dplyr::select(.data$X, .data$Y) %>%
#     tidyr::unite(col= "coordinates",sep = ",") %>%
#     dplyr::mutate(coordinates = paste("[", .data$coordinates, "]",sep="")) %>%
#     dplyr::summarise(coordinates =  paste(.data$coordinates,collapse = ",")) %>%
#     dplyr::mutate(coordinates = paste("{", geometry_type, ":",left_bracket,
#                                       .data$coordinates,
#                                       right_bracket,
#                                       ",'spatialReference':{'wkid':", crs,
#                                       "}}", sep = "")) %>%
#     dplyr::pull()
#   return(out)
# }


format_coords <- function(sf_obj, geom_type) {
  geometry_type <- switch(
    geom_type,
    multipoint = "points",
    polyline = "paths",
    polygon = "rings"
  )
  left_bracket <- "["
  right_bracket <- "]"
  # left_bracket <- switch(
  #   geom_type,
  #   multipoint = "",
  #   polyline = "[",
  #   polygon = "[")
  # right_bracket <- switch(
  #   geom_type,
  #   multipoint = "",
  #   polyline = "]",
  #   polygon = "]")

  geometry_type <- sprintf("'%s'", geometry_type)
  crs <- get_sf_crs(sf_obj)
  out <- lapply(1:nrow(sf_obj), function(x) {
    out <-
      sf_obj[x, ] %>%
      sf::st_coordinates() %>%
      data.frame() %>%
      dplyr::select(.data$X, .data$Y) %>%
      tidyr::unite(col= "coordinates",sep = ",") %>%
      dplyr::mutate(coordinates = paste0("[", .data$coordinates, "]")) %>%
      dplyr::summarise(coordinates =  paste(.data$coordinates, collapse = ","))
  }
  )
  out <-
    do.call("rbind", out) %>%
    dplyr::mutate(coordinates = dplyr::case_when(
      geom_type == "multipoint" ~ .data$coordinates,
      TRUE ~ paste0("[", .data$coordinates, "]")
    )) %>%
    dplyr::pull() %>%
    paste(collapse = ", ")
  out <-
    paste("{", geometry_type, ":",
          left_bracket, out, right_bracket,
          ", 'spatialReference':{'wkid':", crs,
          "}}", sep = "")
  return(out)
}


#' Return CRS value of an sf object
#'
#' @param sf_obj An object of class sf
#'
#' @return A numeric value referring to the coordinate reference system
#' @export
#'
#' @examples
#' get_sf_crs(iceland_poly)
get_sf_crs <- function(sf_obj) {
  stopifnot("sf" %in% class(sf_obj))
  out_crs <- as.numeric(gsub(".*([0-9]{4})$", "\\1",sf::st_crs(sf_obj)[[1]]))
  return(out_crs)
}

#' Create sf objects from coordinates
#'
#' These are simple wrapper functions for creating sf objects from points
#'
#' @param ... The coordinates of the object
#' @param crs The coordinate reference system. Defaults to 4326
#'
#' @return An sf object of the appropriate type
#' @export
#' @name sf_objects
#'
#' @examples
#' pt_a <- c(-90, 45)
#' pt_b <- c(-89, 44)
#' pt <- sf_points(pt_a)
#' line <- sf_line(pt_a, pt_b)
sf_line <- function(..., crs = 4326) {
  coords <- do.call("rbind", list(...))
  ls <- sf::st_linestring(coords)
  sfc <- sf::st_sfc(ls, crs = crs)
  sf <- sf::st_sf(geom = sfc)
  return(sf)
}

#' @rdname sf_objects
#' @export
sf_point <- function(..., crs = 4326) {
  args <- list(...)
  if (length(args) > 1) {
    return(sf_points(..., crs = crs))
  } else if (length(args[[1]]) > 2) {
    return(sf_points(..., crs = crs))
  } else {
    sf_pt <- sf::st_point(...)
    sfc <- sf::st_sfc(sf_pt, crs = crs)
    sf <- sf::st_sf(geom = sfc)
    return(sf)
  }
}

#' @rdname sf_objects
#' @export
sf_points <- function(..., crs = 4326) {
  coords <- do.call("rbind", list(...))
  sf_pts <- sf::st_multipoint(coords)
  sfc <- sf::st_sfc(sf_pts, crs = crs)
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

#' @rdname sf_objects
#' @export
#' @param xmin,xmax,ymin,ymax Corners for sf_box
sf_box <- function(xmin, ymin, xmax, ymax, crs = 4326) {
  a <- c(xmin, ymin)
  b <- c(xmin, ymax)
  c <- c(xmax, ymax)
  d <- c(xmax, ymin)
  return(sf_polygon(a,b,c,d,a, crs = crs))
}


#' Format a SQL where clause from arguments
#'
#' This function will create a where statement that is compatible with
#' \code{\link{get_spatial_layer}}). This statement can then be passed
#' to the \code{where} argument in this function.
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
#' \dontrun{
#' wbics <- sql_where(WATERBODY_WBIC = c(805400, 804600), rel_op = "IN")
#' lakes <- get_spatial_layer(wi_hydro_url, where = wbics)
#' plot_layer(lakes)
#' }
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
      return(paste(names(args)[x], "IN", x_vals))
    } else {
      if (is.character(args[[x]])) {
        x_vals <- paste0("'", args[[x]], "'")
      } else {
        x_vals <- args[[x]]
      }
      return(paste(names(args)[x], rel_op[x], x_vals))
    }
  })
  if (length(out_list) > 1) {
    out <- paste(out_list, collapse = " AND ")
  } else {
    out <- out_list[[1]]
  }
  return(out)
}

#' Pull the HTML body from a web page
#'
#' Used internally to pull HTML for a layer's web page so that the call
#' doesn't have to be made twice in \code{\link{get_geometry_type}} if the
#' url provided there is for a raster layer.
#'
#' @param url Character. The URL of the web page
#'
#' @return A character string of the HTML body
get_layer_html <- function(url) {
  layer_html <-
    xml2::read_html(url) %>%
    rvest::html_nodes("body") %>%
    rvest::html_text() %>%
    stringr::str_replace("\\s+", " ")
  return(layer_html)
}


#' Get elements of a Service or Layer from an ArcGIS REST endpoint
#'
#' This family of functions is meant to pull attributes from a particular
#' service or layer hosted on an ArcGIS REST API. If the service is an
#' ImageServer or MapServer, then the behavior will be slightly different than
#' for a Feature Layer (see details).
#'
#' \code{get_service_type} will return the type of service or layer for the
#' respective URL (or html) that is passed to the function. For a feature layer
#' the function should return "feature_layer", for a Image or Map Server the
#' function will return "image" or "map", respectively.
#'
#' \code{get_geometry_type} will return the geometry type of feature service
#' layers housed on an ArcGIS REST API server. If a URL is provided that points
#' to a map or image layer the function will return an error (i.e. only
#' feature layers have geometry types).
#'
#' \code{get_supported_operations} will simply return a character vector that
#' lists the supported operations for \code{url}.
#'
#' @param url A character string of a valid layer URL
#' @param ... Only used internally, but html can be passed
#'
#' @return A character string defining the layer type
#' @export
#'
#' @examples
#' \dontrun{
#' get_service_type(reykjanes_lava_flow_url)
#' }
get_service_type <- function(url, ...) {
  if (!(requireNamespace("xml2", quietly = TRUE) ||
        requireNamespace("rvest", quietly = TRUE))) {
    stop(
      "You must have xml2, rvest, and stringr installed ",
      "to use get_service_type"
    )
  }
  find_layer_type <- function(html) {
    so <- get_supported_operations(html = html)
    if (any(grepl("Export Image", so))) {
      out <- "image"
    } else if (any(grepl("Export Map", so))) {
      out <- "map"
    } else {
      out <-
        html %>%
        stringr::str_match(base::paste0("Type:\\s+","(.*?) ")) %>%
        stringr::str_replace("Type:", "") %>%
        stringr::str_trim()
      out <- paste0(tolower(out[2]), "_layer")
    }
    return(out)
  }
  # check to see if html is passed; used for checking raster layer type
  # in get_geometry_type
  args <- list(...)
  if ("html" %in% names(args)) {
    geom_type <-
      args$html %>%
      find_layer_type()
  } else if (httr::http_error(url) == TRUE) {
    return("url_error")
  } else {
    geom_type <-
      get_layer_html(url) %>%
      find_layer_type()
  }
  return(geom_type)
}

#' Get Geometry Type
#'
#'
#'
#' @param url A character string of a feature services URL
#'
#' @return A character string of the layers geometry type
#' @export
#'
#' @examples
#' \dontrun{
#' get_geometry_type(reykjanes_lava_flow_url)
#' }
get_geometry_type <- function(url) {
  if (!(requireNamespace("xml2", quietly = TRUE) ||
        requireNamespace("rvest", quietly = TRUE))) {
    stop(
      "You must have xml2, rvest, and stringr installed ",
      "to use get_geometry_type"
    )
  }
  if (httr::http_error(url) == TRUE) {
    "url_error"
  } else {
    layer_html <- get_layer_html(url)
    service_type <- get_service_type(html = layer_html)
    if (service_type != "feature_layer") {
      stop("This is not a Feature Service layer. Geometry Type is only\n",
           "  defined for Feature Service Layers. Otherwise it's probably",
           " a raster")
    } else {
      geom_type <-
        layer_html %>%
        stringr::str_match(base::paste0("Geometry Type: ","(.*?) ")) %>%
        stringr::str_replace("Description:", "") %>%
        stringr::str_trim()
      return(geom_type[2])
    }
  }
}


get_supported_operations <- function(url, ...) {
  if (!(requireNamespace("xml2", quietly = TRUE) ||
        requireNamespace("rvest", quietly = TRUE))) {
    stop(
      "You must have xml2, rvest, and stringr installed ",
      "to use get_service_type"
    )
  }

  extract_so <- function(html) {
    out <-
      html %>%
      stringr::str_extract("Supported Operations:(\\s*.*)*$") %>%
      stringr::str_trim() %>%
      stringr::str_split("\r\n\\s+") %>%
      unlist()
    out <- grep("Supported Operations:", out, value = TRUE, invert = TRUE)
    return(out)
  }

  # check to see if html is passed; used for checking in other functions
  args <- list(...)
  if ("html" %in% names(args)) {
    supp_op <- extract_so(args$html)
  } else {
    supp_op <- extract_so(get_layer_html(url))
  }
  return(supp_op)
}

get_raster_layers <- function(url) {
  layer_html <- get_layer_html(url)
}

#' Lookup function for shorthand versions of spatial relation text strings
#'
#' After typing "esriSpatialRelIntersects" into 4 to 5 functions, you'll get
#' pretty sick of typing that. This function serves to allow shorthand strings
#' to be passed to the \code{sp_rel} arguments of the
#' \code{\link{get_layers_by_spatial}} family of functions. For example, you can
#' pass  "intersects" to this function and it will return
#' "esriSpatialRelIntersects"
#'
#' @param x A character string. One of "contains", "crosses",
#' "envelopeintersects", "indexintersects", "intersects", "overlaps",
#' "relation", "touches", "within"
#'
#' @return The appropriately named ESRI version of \code{x}. For example,
#' an \code{x} value of "intersects" returns "esriSpatialRelIntersects"
#' @export
#'
#' @examples
#' sp_rel_xref("intersects")
sp_rel_xref <- function(x) {
  if (x %in% sp_rel_ref$sp.rel.ref) {
    return(x)
  } else {
    reg_x <- paste0("^", gsub("e?s$", "", x))
    sp_rel_ind <- grep(reg_x, sp_rel_ref$sp.xref)
    out <- sp_rel_ref$sp.rel.ref[sp_rel_ind]
    if (length(out) == 0) {
      stop(paste("Could not find a spatial relationship for", x))
    }
    return(out)
  }
}

#' Check to see which spatial relation types are applicable to the feature
#' classes being queried and the sf objects use do to a spatial query
#'
#' @param fc1 Character. The feature class type being queried. Available options
#' are "point", "multipoint", "line", or "area".
#' @param fc2 Character. The geometry type of the sf object used to do a spatial
#' query. Available options are "point", "multipoint", "line", or "area".
#' @param pull Logical. Pull the available options (TRUE) or print all columns
#' of the sp_rel_valid data.frame for the appropriate fc1 and fc2
#'
#' @return Either a vector or filtered data.frame showing the appropriate
#' sp_rels for the given feature classes
#' @export
#'
#' @examples
#' valid_sp_rel("line", "line")
valid_sp_rel <- function(fc1, fc2, pull = TRUE) {
  fc1 <- tolower(fc1)
  fc2 <- tolower(fc2)
  out <-
    arcpullr::sp_rel_valid %>%
    dplyr::filter(.data$feature_class == fc1, .data$query_feature_class == fc2)
  if (nrow(out) == 0) {
    stop("One of the supplied feature classes cannot be found.")
  }
  if (pull) {
    out <- dplyr::pull(out, .data$sp_rel)
  }
  return(out)
}
