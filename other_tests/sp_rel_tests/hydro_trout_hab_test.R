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

# feature classes to query------------------------------------------------------

# points - trout habitat sites
trout_hab_url <- dnr_url(
  "FM_Trout/FM_TROUT_HAB_SITES_WTM_Ext/MapServer/0"
)

# lines - hydro flowlines
hydro_lines_url <- dnr_url(
  "TS_AGOL_STAGING_SERVICES/EN_AGOL_STAGING_SurfaceWater_WTM/MapServer/2",
  base = 2
)

# polygons - hydro poly
hydro_poly_url <- dnr_url(
  "WT_SWDV/WT_Inland_Water_Resources_WTM_Ext_v2/MapServer/3"
)

query_urls <- tribble(
  ~fc_type, ~url,
  "line", hydro_lines_url,
  "point", trout_hab_url,
  "area", hydro_poly_url
)

# feature classes used for spatial query----------------------------------------

# feature classes used to query points------------------------------------------
trout_hab_sites <-
  get_spatial_layer(trout_hab_url) %>%
  st_transform(crs = 3071) %>%
  select(OBJECTID)

pt_query <-
  trout_hab_sites %>%
  slice(3L) %>%
  mutate(query_fc_type = "point")

multipt_query <-
  trout_hab_sites %>%
  slice(1L:10L) %>%
  mutate(query_fc_type = "multipoint",
         geoms = st_combine(.)) %>%
  slice(1L)

# line query layer
line_query <-
  sf_line(
    c(590000, 521107.5554860905),
    c(500000, 521107.5554860905),
    crs = 3071
  ) %>%
  mutate(query_fc_type = "line")

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

# join point querying layers together
pt_query_layers <- tribble(
  ~query_fc_type, ~query_fc_object,
  "line", line_query,
  "point", pt_query,
  "multipoint", multipt_query,
  "area", poly_query,
  "envelope", env_query
) %>% mutate(fc_type = "point")

# feature classes used to query lines-------------------------------------------

pt_query <-
  sf_point(
    c(563537.9561000001, 332694.98699999973),
    crs = 3071
  ) %>%
  mutate(query_fc_type = "point")

multipt_query <-
  sf_point(
    c(563537.9561000001, 332694.98699999973),
    c(424116.58779999986, 272244.6537999995),
    c(565157.0016999999, 332375.94370000064),
    crs = 3071
  ) %>%
  mutate(query_fc_type = "multipoint")

line_query <-
  sf_line(c(-90, 45), c(-89.7, 45)) %>%
  mutate(query_fc_type = "line")

poly_query <-
  sf_polygon(
    c(-90, 45),
    c(-89.9, 45),
    c(-89.9, 44.9),
    c(-90, 44.9),
    c(-90, 45)
  ) %>%
  mutate(query_fc_type = "area")

env_query <- mutate(poly_query, query_fc_type = "envelope")

# join line querying layers together
line_query_layers <- tribble(
  ~query_fc_type, ~query_fc_object,
  "line", line_query,
  "point", pt_query,
  "multipoint", multipt_query,
  "area", poly_query,
  "envelope", env_query
) %>% mutate(fc_type = "line")

# feature classes used to query polygons----------------------------------------

# I think just do lake mendota here
pt_query <-
  sf_point(c(-89.4, 43.1)) %>%
  mutate(query_fc_type = "point")

multipt_query <-
  sf_points(c(-89.4, 43.1), c(-89.42650, 43.14465), c(-89.4895, 43.1382)) %>%
  mutate(query_fc_type = "multipoint")

line_query <-
  sf_line(c(-89.5, 43.1), c(-89.35, 43.1)) %>%
  mutate(query_fc_type = "line")

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
) %>% mutate(fc_type = "area")


# put everything together in tibbles and run tests------------------------------
query_layers <- list(
    pt_query_layers,
    line_query_layers,
    poly_query_layers
  ) %>%
  do.call("rbind", .)


fc_sp_rel_test <- expand_grid(
  fc_type = c("line", "point", "area"),
  query_fc_type = c("line", "point", "multipoint", "area", "envelope"),
  sp_rel = sp_rels_to_test
) %>%
  left_join(query_urls, by = "fc_type") %>%
  left_join(query_layers, by = c("fc_type", "query_fc_type")) %>%
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
      Sys.sleep(0.1)
      return(suppressWarnings(do.call(f, list(x, y, z))))
    }),
    valid = map_lgl(service_object, ~nrow(tibble(.x)) >= 1)
  )

save(
  test_results,
   file = "other_tests/sp_rel_tests/hydro_trout_hab_test_results.RData"
)
