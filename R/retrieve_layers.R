#' Retrieve a spatial layer from an ArcGIS REST API
#'
#' This function retrieves spatial layers present an ArcGIS
#' REST services API located at.
#'
#' This is the core function of this package. It retrieves spatial layers from
#' an ArcGIS REST API designated by the URL. Additional querying features can
#' be passed such as a SQL WHERE statement (\code{where} argument) as well as
#' spatial queries or any other types of queries that the ArcGIS REST API
#' accepts using \code{...}. However, for easier spatial querying see
#' \code{\link{get_layers_by_spatial}}.
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
#' @param ... Additional arguements to pass to the ArcGIS REST POST request
#'
#' @return
#' An object of class "sf" of the appropriate layer
#'
#' @export
#'
#' @examples
#' # lava flows on Reykjanes (pr. 'rake-yah-ness') peninsula in Iceland
#' base_url <- "https://arcgisserver.isor.is:6443/arcgis/rest/services"
#' reykjanes_path <- "vi/uttekt_eldstodva_allt_2/MapServer/5"
#' reykjanes_url <- paste(base_url, reykjanes_path, sep = "/")
#' lava_flows <- get_spatial_layer(reykjanes_url)
#' plot_layer(lava_flows, outline_poly = reykjanes)
#' \dontrun{
#'   plot_layer(lava_flows, outline_poly = reykjanes) +
#'   ggplot2::geom_sf(data = iceland_poly, fill = NA)
#' }
#'
get_spatial_layer <- function(url,
                              out_fields = c("*"),
                              where = "1=1",
                              token = "",
                              sf_type = NULL,
                              ...) {
  layer_info <- jsonlite::fromJSON(
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
  # print(geomType)
  query_url <- paste(url, "query", sep="/")
  esri_features <- get_esri_features(query_url, out_fields, where, token, ...)
  simple_features <- esri2sfGeom(esri_features, sf_type)
  return(simple_features)
}

get_esri_features <- function(query_url, fields, where, token='', ...) {
  ids <- get_object_ids(query_url, where, token, ...)
  if(is.null(ids)){
    warning("No records match the search critera")
    return()
  }
  id_splits <- split(ids, ceiling(seq_along(ids)/500))
  results <- lapply(
    id_splits,
    get_esri_features_by_id,
    query_url,
    fields,
    token
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
    return(sf::st_point(unlist(feature$geometry)))
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
    return(paths2multiline(feature$geometry$paths))
  }
  geoms <- sf::st_sfc(lapply(features, getGeometry))
  return(geoms)
}
