#' Various example sf polygons
#'
#' These are sf polygons that are used for examples throughout the package
#'
#' @format An object of class sf and data.frame:
#' @name sf_example_polys
#'
#' @source \code{\link[ggplot2]{map_data}}
"iceland"

#' @rdname sf_example_polys
"mke_county"

#' @rdname sf_example_polys
"portage_county"

#' @rdname sf_example_polys
"reykjanes"

#' @rdname sf_example_polys
"wis_counties"

#' @rdname sf_example_polys
"wis_poly"

#' Tables used to lookup and explain which spatial relation types go with
#' different spatial queries
#'
#' sp_rel_lookup explains the various different types of spatial relationships
#' available through ArcGIS REST APIs. sp_rel_valid shows which spatial
#' relationships are valid with different geometry types being queried and
#' used to do spatial queries
#'
#' @format
#' @name sp_rel_lookups
#'
#' @source sp_rel_valid -- \url{
#'   https://desktop.arcgis.com/en/arcmap/latest/extensions/
#'   data-reviewer/types-of-spatial-relationships-that-can-be-validated.htm
#' }
"sp_rel_valid"

#' @rdname sp_rel_lookups
#' @format
#' @source sp_rel_lookup -- \url{
#'   https://help.arcgis.com/en/webapi/wpf/apiref/
#'   ESRI.ArcGIS.Client~ESRI.ArcGIS.Client.Tasks.SpatialRelationship.html
#' }
"sp_rel_lookup"
