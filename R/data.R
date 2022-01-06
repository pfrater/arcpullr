#' Various example sf polygons
#'
#' These are sf polygons that are used for examples throughout the package
#'
#' @format An object of class sf and data.frame:
#' @name sf_example_polys
#'
#' @source \code{\link[ggplot2]{map_data}}
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
