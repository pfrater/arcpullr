#' Retrieve a feature service layer from an ArcGIS REST API
#'
#' This function retrieves spatial layers present in Feature Service layers of
#' an ArcGIS REST services API and returns them as an \code{sf} object
#'
#' This is one of the core functions of this package. It retrieves spatial
#' layers from feature services of an ArcGIS REST API designated by the URL.
#' Additional querying features can be passed such as a SQL WHERE statement
#' (\code{where} argument) or spatial queries as well as any other types of
#' queries that the ArcGIS REST API accepts (using \code{...}). However, for
#' easier spatial querying see \code{\link{get_layers_by_spatial}}.
#'
#' All of the querying parameters are sent via a POST request to the URL, so
#' if there are issues with passing additional parameters via \code{...}
#' first determine how they fit into the POST request and make adjustments as
#' needed. This syntax can be tricky if you're not used to it.
#'
#' @param url A character string of the url for the layer to pull
#' @param out_fields A character string of the fields to pull for each layer
#' @param where A character string of the where condition. Default is 1=1
#' @param token A character string of the token (if needed)
#' @param sf_type A character string specifying the layer geometry to convert to
#' sf ("esriGeometryPolygon", "esriGeometryPoint", "esriGeometryPolyline"),
#' if NULL (default) the server will take its best guess
#' @param head Logical or numeric. Limits the number of records returned from a
#' query. If TRUE, only the first 5 records will be returned. If numeric, then
#' the number of records specified in \code{head} will be returned
#' @param ... Additional arguments to pass to the ArcGIS REST POST request (or
#' associated internal functions used to query them)
#'
#' @return
#' An object of class "sf" of the appropriate layer
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # lava flows on Reykjanes (pr. 'rake-yah-ness') peninsula in Iceland
#' lava_flows <- get_spatial_layer(reykjanes_lava_flow_url)
#' plot_layer(lava_flows, outline_poly = reykjanes_poly)
#' plot_layer(lava_flows, outline_poly = iceland_poly)
#' }
get_spatial_layer <- function(url,
                              out_fields = c("*"),
                              where = "1=1",
                              token = "",
                              sf_type = NULL,
                              head = FALSE,
                              ...) {
  layer_info <- get_layer_info(url, token)
  if (layer_info$type == "Group Layer") {
    stop("You are trying to access a group layer. Please select one of the ",
         "sub-layers instead.")
  }
  if (is.null(sf_type)) {
    if (is.null(layer_info$geometryType))
      stop("return_geometry is NULL and layer geometry type \n(e.g. ",
           "'esriGeometryPolygon' or ",
           "'esriGeometryPoint' or ",
           "'esriGeometryPolyline' ",
           ")\ncould not be infered from server.")
    sf_type <- layer_info$geometryType
  }
  query_url <- paste(url, "query", sep="/")
  args <- list(query_url, out_fields, where, token, head, ...)
  if(!"idsplits" %in% names(args))
    args <- c(args, list(idsplits = layer_info$maxRecordCount))
  esri_features <- do.call(get_esri_features, args)
  simple_features <- esri2sfGeom(esri_features, sf_type)
  return(simple_features)
}

#' Retrieve a table from an ArcGIS REST API
#'
#' This function retrieves tables present in an ArcGIS REST services API and
#' returns them as a data frame.
#'
#' This function retrieves tables from an ArcGIS REST API designated by the
#' URL. Additional querying features can be passed such as a SQL WHERE
#' statement (\code{where} argument) as well as any other types of queries
#' that the ArcGIS REST API accepts (using \code{...}).
#'
#' All of the querying parameters are sent via a POST request to the URL, so
#' if there are issues with passing additional parameters via \code{...}
#' first determine how they fit into the POST request and make adjustments as
#' needed. This syntax can be tricky if you're not used to it.
#'
#' @param url A character string of the url for the layer to pull
#' @param out_fields A character string of the fields to pull for each layer
#' @param where A character string of the where condition. Default is 1=1
#' @param token A character string of the token (if needed)
#' @param head Logical or numeric. Limits the number of records returned from a
#' query. If TRUE, only the first 5 records will be returned. If numeric, then
#' the number of records specified in \code{head} will be returned
#' @param ... Additional arguments to pass to the ArcGIS REST POST request (or
#' associated internal functions used to query them)
#'
#' @return A data frame of the appropriate layer
#' @export
get_table_layer <- function(url,
                            out_fields = "*",
                            where = "1=1",
                            token = "",
                            head = FALSE,
                            ...) {
  query_url <- paste(url, "query", sep = "/")
  layer_info <- get_layer_info(url, token)
  args <- list(query_url, out_fields, where, token, head, ...)
  if(!"idsplits" %in% names(args))
    args <- c(args, list(idsplits = layer_info$maxRecordCount))
  esri_features <- do.call(get_esri_features, args)
  atts <-
    lapply(esri_features, "[[", 1) %>%
    lapply(
      function(att)
        lapply(att, function(x) ifelse(is.null(x), NA, x))
    )
  dplyr::bind_rows(lapply(atts, as.data.frame.list, stringsAsFactors = FALSE))
}

#' Retrieve metadata for a layer
#'
#' This function retrieves metadata for a layer.
#'
#' @param url A character string of the url for the layer to pull
#' @param token A character string of the token (if needed)
#'
#' @return A list of metadata fields
#' @export
#'
#' @examples
#' \dontrun{
#' # lava flows on Reykjanes (pr. 'rake-yah-ness') peninsula in Iceland
#' lava_flows_info <- get_layer_info(reykjanes_lava_flow_url)
#' }
get_layer_info <- function(url, token = "") {
  jsonlite::fromJSON(
    httr::content(
      httr::POST(
        url,
        query=list(f="json", token=token),
        encode="form",
        config = httr::config(ssl_verifypeer = FALSE)
      ),
      as="text"
    )
  )
}

get_esri_features <- function(query_url, fields, where, token='', head, ...) {
  ids <- get_object_ids(query_url, where, token, ...)
  if(is.null(ids)){
    warning("No records match the search critera")
    return()
  }
  if (!("idsplits" %in% names(list(...)))) {
    idsplits <- 500
  } else {
    idsplits <- list(...)$idsplits
  }
  if (isTRUE(head)) {
    id_splits <- ids[1:5]
  } else if (head > 0 & head < idsplits) {
    id_splits <- ids[1:head]
  } else {
    id_splits <- split(ids, ceiling(seq_along(ids) / idsplits))
  }
  results <- lapply(
    id_splits,
    get_esri_features_by_id,
    query_url,
    fields,
    token,
    ...
  )
  merged <- unlist(results, recursive=FALSE)
  return(merged)
}

get_object_ids <- function(query_url, where, token='', ...){
  # create Simple Features from ArcGIS servers json response
  query <- list(
    where=where,
    returnIdsOnly="true",
    token=token,
    f="json",
    ...
  )
  response_raw <- httr::content(
    httr::POST(
      query_url,
      body=query,
      encode="form",
      config = httr::config(ssl_verifypeer = FALSE)),
    as="text"
  )
  response <- jsonlite::fromJSON(response_raw)
  return(response$objectIds)
}

get_esri_features_by_id <- function(ids, query_url, fields, token='', ...){
  # create Simple Features from ArcGIS servers json response
  query <- list(
    objectIds=paste(ids, collapse=","),
    outFields=paste(fields, collapse=","),
    token=token,
    outSR='4326',
    f="json",
    ...
  )
  response_raw <- httr::content(
    httr::POST(
      query_url,
      body=query,
      encode="form",
      config = httr::config(ssl_verifypeer = FALSE)
    ),
    as="text"
  )
  response <- jsonlite::fromJSON(response_raw,
                                 simplifyDataFrame = FALSE,
                                 simplifyVector = FALSE,
                                 digits=NA)
  esri_json_features <- response$features
  return(esri_json_features)
}

esri2sfGeom <- function(jsonFeats, sf_type) {
  # convert esri json to simple feature
  if (sf_type == 'esriGeometryPolygon') {
    geoms <- esri2sfPolygon(jsonFeats)
  }
  if (sf_type == 'esriGeometryPoint') {
    geoms <- esri2sfPoint(jsonFeats)
  }
  if (sf_type == 'esriGeometryPolyline') {
    geoms <- esri2sfPolyline(jsonFeats)
  }
  # attributes
  atts <- lapply(jsonFeats, '[[', 1) %>%
    lapply(function(att)
      lapply(att,
             function(x) {
               return(ifelse(is.null(x), NA, x))
             }
      )
    )
  af <- dplyr::bind_rows(
    lapply(
      atts,
      as.data.frame.list,
      stringsAsFactors=FALSE
    )
  )
  # geometry + attributes
  df <- sf::st_sf(geoms, af, crs="+init=epsg:4326")
  return(df)
}

esri2sfPoint <- function(features) {
  getPointGeometry <- function(feature) {
    if (is.null(feature$geometry)) {
      return(sf::st_point())
    } else {
      return(sf::st_point(unlist(feature$geometry)))
    }
  }
  geoms <- sf::st_sfc(lapply(features, getPointGeometry))
  return(geoms)
}

esri2sfPolygon <- function(features) {
  ring2matrix <- function(ring) {
    return(do.call(rbind, lapply(ring, unlist)))
  }
  rings2multipoly <- function(rings) {
    return(sf::st_multipolygon(list(lapply(rings, ring2matrix))))
  }
  getGeometry <- function(feature) {
    if(is.null(unlist(feature$geometry$rings))){
      return(sf::st_multipolygon())
    } else {
      return(rings2multipoly(feature$geometry$rings))
    }
  }
  geoms <- sf::st_sfc(lapply(features, getGeometry))
  return(geoms)
}

esri2sfPolyline <- function(features) {
  path2matrix <- function(path) {
    return(do.call(rbind, lapply(path, unlist)))
  }
  paths2multiline <- function(paths) {
    return(sf::st_multilinestring(lapply(paths, path2matrix)))
  }
  getGeometry <- function(feature) {
    if (is.null(unlist(feature$geometry$paths))) {
      return(sf::st_multilinestring())
    } else {
      return(paths2multiline(feature$geometry$paths))
    }
  }
  geoms <- sf::st_sfc(lapply(features, getGeometry))
  return(geoms)
}

#' Retrieve a map service layer from an ArcGIS REST API
#'
#' This function retrieves map service layers from an ArcGIS
#' REST services API and returns them as a \code{RasterLayer} object
#'
#' This is one of the core functions of the package. It retrieves map service
#' layers from an ArcGIS REST API designated by the URL. These layers require a
#' bounding box to query the map layer, which is either taken from the
#' \code{sf_object} argument or optionally can be passed via the \code{bbox}
#' argument. Either \code{sf_object} or \code{bbox} are optional, but one of
#' them must be present.
#'
#' All of the querying parameters are sent via a POST request to the URL, so
#' if there are issues with passing additional parameters via \code{...}
#' first determine how they fit into the POST request and make adjustments as
#' needed. This syntax can be tricky if you're not used to it.
#'
#' @inheritParams get_raster_layer
#'
#' @return A "RasterLayer" object
#' @export
#'
#' @examples
#' \dontrun{
#' wi_landcover<- get_map_layer(wi_landcover_url, wis_poly)
#' plot_layer(wi_landcover, outline_poly = wis_poly)
#' }
get_map_layer <- function(url,
                          sf_object = NULL,
                          bbox = NULL,
                          bbox_crs = NULL,
                          token = "",
                          clip_raster = TRUE,
                          format = "png",
                          transparent = TRUE,
                          add_legend = TRUE,
                          ...) {
  out <- get_raster_layer(
    url = url,
    sf_object = sf_object,
    bbox = bbox,
    bbox_crs = bbox_crs,
    token = token,
    clip_raster = clip_raster,
    format = format,
    transparent = transparent,
    export_type = "map",
    add_legend = add_legend,
    ...
  )
  return(out)
}


#' Retrieve an image service layer from an ArcGIS REST API
#'
#' This function retrieves image service layers from an ArcGIS
#' REST services API and returns them as a \code{RasterStack} object
#'
#' This is one of the core functions of the package. It retrieves image service
#' layers from an ArcGIS REST API designated by the URL. These layers require a
#' bounding box to query the map layer, which is either taken from the
#' \code{sf_object} argument or optionally can be passed via the \code{bbox}
#' argument. Either \code{sf_object} or \code{bbox} are optional, but one of
#' them must be present.
#'
#' All of the querying parameters are sent via a POST request to the URL, so
#' if there are issues with passing additional parameters via \code{...}
#' first determine how they fit into the POST request and make adjustments as
#' needed. This syntax can be tricky if you're not used to it.
#'
#' @inheritParams get_raster_layer
#'
#' @return A "RasterStack" object
#' @export
#'
#' @examples
#' \dontrun{
#' wi_leaf_off_layer <- get_image_layer(wi_leaf_off_url, wis_poly)
#' plot_layer(wi_leaf_off_layer, outline_poly = wis_poly)
#' }
get_image_layer <- function(url,
                            sf_object = NULL,
                            bbox = NULL,
                            bbox_crs = NULL,
                            token = "",
                            clip_raster = TRUE,
                            format = "png",
                            transparent = TRUE,
                            ...) {
  out <- get_raster_layer(
    url = url,
    sf_object = sf_object,
    bbox = bbox,
    bbox_crs = bbox_crs,
    token = token,
    clip_raster = clip_raster,
    format = format,
    transparent = transparent,
    export_type = "image",
    ...
  )
  return(out)
}


#' Pull a raster layer from a map service or image service layer of an ArcGIS
#' REST API
#'
#' This is an internal function to pull raster layers from either a map service
#' or an image service of an ArcGIS REST API. This function is the engine that
#' drives \code{\link{get_map_layer}} and \code{\link{get_image_layer}}
#'
#' @param url A character string of the url for the layer to pull
#' @param sf_object An \code{sf} object used for the bounding box
#' @param bbox Vector of bounding box coordinates
#' @param bbox_crs CRS for bbox (required if bbox is used)
#' @param token A character string of the token (if needed)
#' @param clip_raster Logical. Should the raster be clipped to contain only
#' the pixels that reside in the \code{sf_object}? By default, ArcGIS returns
#' some overlapping edge pixels. Setting \code{clip_raster} to TRUE (default)
#' will remove these using \code{\link[raster]{mask}} from the \code{raster}
#' package
#' @param format The raster format desired. Default is "png"
#' @param transparent Logical. Retrieve a raster with a transparent background
#' (TRUE, default) or not (FALSE)
#' @param export_type Character. Either "map" or "image" for the respective
#' service layer desired
#' @param add_legend Logical. Pull legend and match to color values
#' (TRUE, default) or not (FALSE)
#' @param ... Additional arguments to pass to the ArcGIS REST API
#'
#' @return An object of type \code{RasterLayer} if \code{export_type = "map"} or
#' an object of type \code{RasterStack} if \code{export_type = "image"}
get_raster_layer <- function(url,
                             sf_object = NULL,
                             bbox = NULL,
                             bbox_crs = NULL,
                             token = "",
                             clip_raster = TRUE,
                             format = "png",
                             transparent = TRUE,
                             export_type = "map",
                             add_legend = FALSE,
                             ...) {
  if (is.null(sf_object) && is.null(bbox)) {
    stop(
      "You must specify either an sf_object to spatially query by ",
      "or a bbox as a character string"
    )
  } else if (!is.null(sf_object) && !is.null(bbox)) {
    stop(
      "You must specify either an sf_object or a bbox, but may not specify both"
    )
  } else if (!is.null(sf_object)) {
    bbox <- sf::st_bbox(sf_object)
    bbox_coords <- paste(bbox, collapse = ", ")
    bbox_sr <- get_sf_crs(sf_object)
  } else {
    if (is.null(bbox_crs)) {
      stop("You must specify bbox_crs if you are using the bbox argument")
    } else {
      bbox_sr <- bbox_crs
    }
    bbox_coords <- paste(bbox, collapse = ", ")
  }
  if (export_type == "map") {
    export_url <- paste(url, "export", sep = "/")
  } else if (export_type == "image") {
    if (add_legend) {
      warning("You can only use add_legend with get_map_layer(). ",
              "Setting add_legend to FALSE")
    }
    add_legend <- FALSE
    export_url <- paste(url, "exportImage", sep = "/")
  }
  if (transparent) {
    if (!(grepl("png|gif", format))) {
      transparent <- FALSE
      warning("Transparent background only available for png and gif formats")
    }
  }
  response_raw <- httr::POST(
    url = export_url,
    body = list(
      f = "json",
      token = token,
      bbox = bbox_coords,
      bboxSR = bbox_sr,
      imageSR = bbox_sr,
      transparent = transparent,
      format = format,
      ...
    )
  )
  response <- jsonlite::fromJSON(rawToChar(response_raw$content))
  raster_url <- response$href
  raster_extent <- raster::extent(unlist(response$extent[c(1, 3, 2, 4)]))
  raster_crs <- raster::crs(sf_object)

  # set the extent and projection of the raster layer
  if (export_type == "map") {
    out <- raster::raster(raster_url)
    if (raster::nbands(out) > 1) {
      out <- raster::stack(raster_url)
    }
  } else if (export_type == "image") {
    out <- raster::stack(raster_url)
  }
  raster::extent(out) <- raster_extent
  raster::projection(out) <- raster_crs

  # read the raster into memory (as opposed to a connection)
  out <- raster::readAll(out)

  if (clip_raster) {
    out <- raster::mask(out, sf_object)
  }

  if (add_legend) {
    raster_cols <- raster::colortable(out)
    legend <-
      get_layer_legend(url) %>%
      match_raster_colors(out) %>%
      dplyr::arrange(color = match(.data$color, raster_cols[-1]))
    if (nrow(legend) == length(raster_cols)) {
      legend_names <- legend$value
    } else {
      legend_names <- c(NA, legend$value)
    }
    out@legend@names <- legend_names
  }

  return(out)
}
