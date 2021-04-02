# testing which spatial relations can be used when doing spatial queries

sp_rels <- sp_rel_lookup$sp_rel

hydro_url <- paste0(
  "https://dnrmaps.wi.gov/arcgis2/rest/services/",
  "TS_AGOL_STAGING_SERVICES/EN_AGOL_STAGING_SurfaceWater_WTM/MapServer/0"
  #"WT_SWDV/WT_Inland_Water_Resources_WTM_Ext_v2/MapServer/3"
)

# query for lines---------------------------------------------------------------

# using a line------------------------------------------------------------------
line <- sf_line(c(-90, 45), c(-89, 45))

# test to make sure the default works
streams_on_line <- get_layer_by_line(hydro_url, geometry = line)

# now test each sp_rel type
line_line_test <- lapply(sp_rels, function(x) {
  out <- get_layer_by_line(hydro_url, geometry = line, sp_rel = x)
})
names(line_line_test) <- sp_rels

# sp_rels that did not work
names(line_line_test[Map(length, line_line_test) > 1])

# sp_rel types that are supposed to work
valid_sp_rel("line", "line")
