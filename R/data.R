#' Various example sf polygons
#'
#' These are sf polygons that are used for examples throughout the package
#'
#' @format An object of class sf and data.frame:
#' @name sf_example_polys
#'
#' @source ggplot2's \code{\link[ggplot2]{map_data}} and \href{https://dnrmaps.wi.gov/arcgis/rest/services}{Wisconsin DNR ArcGIS REST API}
"iceland_poly"

#' @rdname sf_example_polys
"mke_county"

#' @rdname sf_example_polys
"portage_county"

#' @rdname sf_example_polys
"reykjanes_poly"

#' @rdname sf_example_polys
"wis_counties"

#' @rdname sf_example_polys
"wis_poly"

#' @rdname sf_example_polys
"cook_creek_ws"

#' @rdname sf_example_polys
"cook_creek_streams"

#' @rdname sf_example_polys
"cook_creek_env"

#' @rdname sf_example_polys
"mke_river"

#' @rdname sf_example_polys
"poly_streams_contains"

#' @rdname sf_example_polys
"poly_streams_crosses"

#' @rdname sf_example_polys
"sugar_creek"

#' @rdname sf_example_polys
"sugar_creek_env"

#' @rdname sf_example_polys
"trout_hab_project_pt"

#' @rdname sf_example_polys
"trout_hab_project_pts"

#' @rdname sf_example_polys
"example_poly"

#' @rdname sf_example_polys
"trout_hab_project_pts"


#' Various example raster objects
#'
#' These are raster objects that are used for examples throughout the package
#'
#' @name sf_example_raster
#'
#' @source \href{https://dnrmaps.wi.gov/arcgis_image/rest/services}{Wisconsin DNR ArcGIS Image Server}
"wi_landcover"


#' @rdname sf_example_raster
"wi_aerial_imagery"


#' Various URLs used in examples
#'
#' These are URLs that are used for examples throughout the package
#'
#' @format Character strings of URLs
#' @name example_urls
"reykjanes_lava_flow_url"

#' @rdname example_urls
#' @export
"wi_hydro_url"

#' @rdname example_urls
#' @export
"wi_landcover_url"

#' @rdname example_urls
#' @export
"wi_leaf_off_url"


#' Spatial relationship descriptor and lookup tables
#'
#'
#' These data.frames are used to lookup and explain which spatial relation types
#' go with different spatial queries.
#'
#' sp_rel_lookup explains the various different types of spatial relationships
#' available through ArcGIS REST APIs. sp_rel_valid shows which spatial
#' relationships are valid with different geometry types being queried and
#' used to do spatial queries
#'
#'
#' @format \code{sp_rel_valid} is a data.frame with 105 rows and 3 variables as
#' follows:
#' \describe{
#'   \item{feature_class}{A feature class to be queried}
#'   \item{query_feature_class}{The feature class used to do a spatial query}
#'   \item{sp_rel}{
#'     The spatial relationships that are valid for the feature class and
#'     query_feature_class combination
#'   }
#' }
#' @name sp_rel_lookups
#'
#' @source sp_rel_valid--Independent tests done specifically by and for arcpullr
"sp_rel_valid"

#' @rdname sp_rel_lookups
#' @format \code{sp_rel_lookup} is a data.frame with 9 rows and 2 variables as
#' follows:
#' \describe{
#'   \item{sp_rel}{The spatial relationship being described}
#'   \item{description}{A description of the sp_rel}
#' }
#' @source sp_rel_lookup --
#' \url{https://help.arcgis.com/en/webapi/wpf/apiref/ESRI.ArcGIS.Client~ESRI.ArcGIS.Client.Tasks.SpatialRelationship.html
#' }
"sp_rel_lookup"
