library(sf)
library(tidyverse)

fc_type <- c("multipoint", "point", "line", "area")
query_fc_type <- c("envelope", "point", "line", "area", "multipoint")
sp_rels_to_test <- sp_rel_lookup$sp_rel

# set up some different layers to query by--------------------------------------

# helper function to create urls
dnr_url <- function(x, base = NULL) {
  return(paste0("https://dnrmaps.wi.gov/arcgis", base, "/rest/services/", x))
}

# first do queried feature class of line and point (these work well together)---

# point/multipoint spatial query layer
trout_hab_path <- "FM_Trout/FM_TROUT_HAB_SITES_WTM_Ext/MapServer/0"
trout_hab_url <- dnr_url(trout_hab_path)
trout_hab_sites <-
  get_spatial_layer(trout_hab_url) %>%
  st_transform(crs = 3071) %>%
  select(OBJECTID)

multipt_query <-
  trout_hab_sites %>%
  slice(1L:10L) %>%
  mutate(query_fc_type = "multipoint",
         geoms = st_combine(.)) %>%
  slice(1L)

pt_query <-
  trout_hab_sites %>%
  slice(2L) %>%
  mutate(query_fc_type = "point")

# line query layer
line_query <-
  sf_line(c(590000, 521107.5554860905),
          c(500000, 521107.5554860905)) %>%
  mutate(query_fc_type = "line")
st_crs(line_query) <- 3071

# poly and envelope query layers
poly_query <- sf_polygon(
  c(-92.05, 44.65),
  c(-91.85, 44.65),
  c(-91.85, 44.6),
  c(-92.05, 44.6),
  c(-92.05, 44.65)
) %>%
  mutate(query_fc_type = "area")

env_query <- mutate(poly_query, query_fc_type = "envelope")

# join query layers together
line_pt_query_layers <- tribble(
  ~query_fc_type, ~query_fc_object,
  "line", line_query,
  "point", pt_query,
  "multipoint", multipt_query,
  "area", poly_query,
  "envelope", env_query
)

# feature classes to query

# lines - hydro flowlines
hydro_lines_url <- dnr_url(
  "TS_AGOL_STAGING_SERVICES/EN_AGOL_STAGING_SurfaceWater_WTM/MapServer/2",
  base = 2
)
line_pt_fc_query_urls <- tribble(
  ~fc_type, ~url,
  "line", hydro_lines_url,
  "point", trout_hab_url
)

# put together into a data.frame
line_pt_test <- expand_grid(
    fc_type = c("line", "point"),
    query_fc_type = c("line", "point", "multipoint", "area", "envelope"),
    sp_rel = sp_rels_to_test
  ) %>%
  left_join(line_pt_fc_query_urls, by = "fc_type") %>%
  left_join(line_pt_query_layers, by = "query_fc_type")



# now do query feature class of polygon-----------------------------------------

# I think just do lake mendota here
line_query <-
  sf_line(c(-89.5, 43.1), c(-89.35, 43.1)) %>%
  mutate(query_fc_type = "line")

pt_query <-
  sf_point(c(-89.4, 43.1)) %>%
  mutate(query_fc_type = "point")

multipt_query <-
  sf_points(c(-89.1, 43.1), c(-89.45, 43.15), c(-89.45, 43.05)) %>%
  mutate(query_fc_type = "multipoint")

poly_query <-
  sf_polygon(
    c(-89.5, 43.15),
    c(-89.35, 43.15),
    c(-89.35, 43.05),
    c(-89.5, 43.05),
    c(-89.5, 43.15)
  ) %>%
  mutate(query_fc_type = "area")

env_query <- mutate(poly_query, query_fc_type = "envelope")

# join query layers together
poly_query_layers <- tribble(
  ~query_fc_type, ~query_fc_object,
  "line", line_query,
  "point", pt_query,
  "multipoint", multipt_query,
  "area", poly_query,
  "envelope", env_query
)


# poly feature layer to query - hydro poly
hydro_poly_url <- dnr_url(
  "WT_SWDV/WT_Inland_Water_Resources_WTM_Ext_v2/MapServer/3"
)
poly_fc_query_urls <- tribble(
  ~fc_type, ~url,
  "area", hydro_poly_url
)

# put together into a data.frame
poly_test <-
  expand_grid(
    fc_type = c("area"),
    query_fc_type = c("line", "point", "multipoint", "area", "envelope"),
    sp_rel = sp_rels_to_test
  ) %>%
  left_join(poly_fc_query_urls, by = "fc_type") %>%
  left_join(poly_query_layers, by = "query_fc_type")


# combine point, line, and polygon test data.frames-----------------------------
fc_sp_rel_test <-
  rbind(line_pt_test, poly_test) %>%
  mutate(query_function = case_when(
    query_fc_type == "line" ~ list(get_layer_by_line),
    query_fc_type == "point" ~ list(get_layer_by_point),
    query_fc_type == "multipoint" ~ list(get_layer_by_multipoint),
    query_fc_type == "area" ~ list(get_layer_by_poly),
    query_fc_type == "envelope" ~ list(get_layer_by_envelope)
  ))

# pull results using each spatial rel
test_results <-
  fc_sp_rel_test %>%
  mutate(service_object = pmap(
    list(query_function, url, query_fc_object, sp_rel),
    function(f, x, y, z) {
      return(suppressWarnings(do.call(f, list(x, y, z))))
    }),
    valid = map_lgl(service_object, ~nrow(tibble(.x)) > 1)
  )

save(
  test_results,
   file = "other_tests/sp_rel_tests/hydro_trout_hab_test_results.RData"
)
