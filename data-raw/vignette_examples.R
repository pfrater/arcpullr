
# layers to pull for spatial_query vignette examples
# 1. milwaukee river
# 2. trout hab projects - don't really love advertising these to the public
#   -what would be a better point layer to query by?
# 3. brown county rivers


#WDNR Server
server <- "https://dnrmaps.wi.gov/arcgis/rest/services/"
server2 <- "https://dnrmaps.wi.gov/arcgis2/rest/services/"

#River URL
layer <- "TS_AGOL_STAGING_SERVICES/EN_AGOL_STAGING_SurfaceWater_WTM/MapServer/2"
river_url <- paste0(server2,layer)

#Country URL
layer <- "DW_Map_Dynamic/EN_Basic_Basemap_WTM_Ext_Dynamic_L16/MapServer/3"
county_url <- paste0(server,layer)

# SNA URL
layer <- "LF_DML/ER_DNR_NA_StateNaturalAreas_WTM_Ext/MapServer/5"
sna_url <- paste0(server, layer)

#Trout URL
layer <- "FM_Trout/FM_TROUT_HAB_SITES_WTM_Ext/MapServer/0"
trout_url <- paste0(server,layer)

#Watershed URL
layer <- "WT_SWDV/WT_Inland_Water_Resources_WTM_Ext_v2/MapServer/5"
watershed_url <- paste0(server,layer)

# 1 - milwaukee river
mke_river <- get_spatial_layer(
    river_url,
    where = sql_where(RIVER_SYS_NAME = "Milwaukee River")
  ) %>%
  dplyr::group_by(RIVER_SYS_NAME, ROW_NAME, RIVER_SYS_WBIC, STREAM_ORDER) %>%
  dplyr::summarize(geoms = sf::st_combine(geoms), .groups = "drop")

# 2. pull some different trout habitat projects
trout_hab_project_pts <- get_spatial_layer(
  trout_url,
  # where = "FISCALYEAR = 2018"
  where = "WATERBODYNAMECOMBINED = 'Sugar Creek' and FISCALYEAR = 2017"
)
trout_hab_project_pt <- trout_hab_project_pts[1, ]
sugar_creek <- get_layer_by_point(river_url, trout_hab_projects_pts)
sugar_creek_env <- get_layer_by_envelope(river_url, sugar_creek)


# use coon creek as example for polygon and envelope
cook_creek_ws <- wdnr.gis::get_watershed_layer(
  watershed_name = "Cook Creek"
)
cook_creek_streams <-
  get_layer_by_poly(river_url, cook_creek_ws) %>%
  dplyr::group_by(ROW_NAME, STREAM_ORDER) %>%
  dplyr::summarize(geoms = sf::st_combine(geoms), .groups = "drop")
cook_creek_env <-
  get_layer_by_envelope(river_url, cook_creek_ws) %>%
  dplyr::group_by(ROW_NAME, STREAM_ORDER) %>%
  dplyr::summarize(geoms = sf::st_combine(geoms), .groups = "drop")

# some example data for contains vs. crosses sp_rel
example_poly <- sf_polygon(
  c(-90.62, 43.76),
  c(-90.62, 43.77),
  c(-90.61, 43.77),
  c(-90.61, 43.76),
  c(-90.62, 43.76)
)
poly_streams_contains <- get_layer_by_poly(river_url, example_poly)
poly_streams_crosses <- get_layer_by_poly(
  river_url,
  example_poly,
  sp_rel = "esriSpatialRelCrosses"
)


usethis::use_data(
  mke_river,
  trout_hab_project_pt,
  trout_hab_project_pts,
  sugar_creek,
  sugar_creek_env,
  cook_creek_streams,
  cook_creek_ws,
  cook_creek_env,
  example_poly,
  poly_streams_contains,
  poly_streams_crosses,
  overwrite = TRUE
)

