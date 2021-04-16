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

# lines - outstanding_streams
outstanding_streams_url <- dnr_url(
  "WT_Condition_Viewer/WT_Water_Quality_Standards_WTM_Ext/MapServer/14"
)

# points - goodyear distributor sites
goodyear_url <- dnr_url(
  "FN_FLEET/FN_FLEET_TIRE_VENDORS_WTM/MapServer/1",
  base = 2
)

# polygons - vertical seawalls
vertical_seawall_url <- dnr_url(
  "WY_Lakes_AIS/WY_Shoreland_Habitat_WTM_Ext/MapServer/20"
)

query_urls <- tribble(
  ~fc_type, ~url,
  "line", outstanding_streams_url,
  "point", goodyear_url,
  "area", vertical_seawall_url
)

# feature classes used for spatial query----------------------------------------


# feature classes used to query points------------------------------------------

# point spatial query layer
goodyear_sites <- get_spatial_layer(goodyear_url)

multipt_query <-
  goodyear_sites %>%
  slice(1L:10L) %>%
  mutate(query_fc_type = "multipoint",
         geoms = st_combine(.)) %>%
  slice(1L)

pt_query <-
  goodyear_sites %>%
  slice(2L) %>%
  mutate(query_fc_type = "point")

# line query layer
line_query <-
  sf_line(c(730000, 462033.9869159721),
          c(720000, 462033.9869159721),
          crs = 3071) %>%
  mutate(query_fc_type = "line")

# poly and envelope query layers
poly_query <- sf_polygon(
    c(-90, 43.25),
    c(-89, 43.25),
    c(-89, 43),
    c(-90, 43),
    c(-90, 43.25)
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
  ) %>%
  mutate(fc_type = "point")


# feature classes used to query lines-------------------------------------------

pt_query <-
  sf_point(
    c(665229.0853000004, 340270.02380000055),
    crs = 3071
  ) %>%
  mutate(query_fc_type = "point")

multipt_query <-
  sf_point(
    c(665229.0853000004, 340270.02380000055),
    c(707709.9249, 443849.74870000035),
    c(707972.0363999996, 453743.5901999995),
    crs = 3071
  ) %>%
  mutate(query_fc_type = "multipoint")

line_query <-
  sf_line(c(-90, 45), c(-89, 45)) %>%
  mutate(query_fc_type = "line")

poly_query <-
  sf_polygon(
    c(-90, 45),
    c(-89, 45),
    c(-89, 44.8),
    c(-90, 44.8),
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
  ) %>%
  mutate(fc_type = "line")


# feature classes used to query polygons----------------------------------------

pt_query <- sf_point(c(-89.7067, 45.71230)) %>% mutate(query_fc_type = "point")

multipt_query <-
  sf_points(
    c(-89.7067, 45.71230),
    c(-89.7064, 45.71135),
    c(-89.7064, 45.71015)
  ) %>%
  mutate(query_fc_type = "multipoint")

line_query <-
  sf_line(c(-89.7066, 45.71230), c(-89.7063, 45.71015)) %>%
  mutate(query_fc_type = "line")

poly_query <- sf_polygon(
    c(-89.72, 45.72),
    c(-89.7, 45.72),
    c(-89.7, 45.7),
    c(-89.72, 45.7),
    c(-89.72, 45.72)
  ) %>%
  mutate(query_fc_type = "area")

env_query <- mutate(poly_query, query_fc_type = "envelope")

# join polygon querying layers together
poly_query_layers <- tribble(
    ~query_fc_type, ~query_fc_object,
    "line", line_query,
    "point", pt_query,
    "multipoint", multipt_query,
    "area", poly_query,
    "envelope", env_query
  ) %>%
  mutate(fc_type = "area")

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
   file = paste(
     "other_tests",
     "sp_rel_tests",
     "goodyear_seawall_outstanding_streams_test_results.RData",
     sep = "/"
   )
)
