## code to prepare `raster_layers` dataset goes here
# WDNR Server
image_server <- "https://dnrmaps.wi.gov/arcgis_image/rest/services/"

# WI Landcover Type URL
landcover_path <- "DW_Land_Cover/EN_Land_Cover2_Lev2/MapServer"
landcover_url <- paste0(image_server, landcover_path)

wi_landcover <- get_map_layer(landcover_url, wis_poly)

wi_leaf_off_path <- "DW_Imagery/EN_Image_Basemap_Latest_Leaf_Off/ImageServer"
wi_aerial_imagery_url <- paste0(image_server, wi_leaf_off_path)
wi_aerial_imagery <- get_image_layer(wi_aerial_imagery_url, wis_poly)

terra::writeRaster(
  wi_landcover,
  here::here("inst/wi_landcover.png"),
  overwrite = TRUE
)
terra::writeRaster(
  wi_aerial_imagery,
  here::here("inst/wi_aerial_imagery.png"),
  overwrite = TRUE
)
# usethis::use_data(wi_landcover, wi_aerial_imagery, overwrite = TRUE)
