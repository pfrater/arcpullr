library(sf)
library(tidyverse)
library(leaflet)

fc_type <- c("multipoint", "point", "line", "area")
query_fc_type <- c("envelope", "point", "line", "area", "multipoint")
sp_rels_to_test <- sp_rel_lookup$sp_rel

# set up some different layers to query by--------------------------------------

# helper function to create urls
brown_url <- function(x) {
  return(paste0("https://gis.co.brown.wi.us/arcgis/rest/services/", x))
}

# feature classes to query------------------------------------------------------

# points - goodyear distributor sites
hydrants_url <- brown_url(
  "Hydrants/MapServer/0"
)

# lines - outstanding_streams
biketrail_url <- brown_url(
  "BikeAndPedestrian/MapServer/1"
)

# polygons - vertical seawalls
parks_url <- brown_url(
  "Parks/MapServer/0"
)

query_urls <- tribble(
  ~fc_type, ~url,
  "line", biketrail_url,
  "point", hydrants_url,
  "area", parks_url
)

# point spatial query layer
hydrants_locations <- get_spatial_layer(hydrants_url)

multipt_query <-
  hydrants_locations %>%
  slice(1L:10L) %>%
  mutate(query_fc_type = "multipoint",
         geoms = st_combine(.)) %>%
  slice(1L)

pt_query <-
  hydrants_locations %>%
  slice(2L) %>%
  mutate(query_fc_type = "point")

# line query layer
line_query <-
  sf_line(c( -88.091755363494997, 44.6),
           c( -88.091755363494997, 44.5 ),
           crs = 4326) %>%
  mutate(query_fc_type = "line")

# poly and envelope query layers
poly_query <- sf_polygon(
  c(-88.1,44.7),
  c(-88.3,44.7),
  c(-88.3,44.1),
  c(-88.1,44.1),
  c(-88.1,44.7),
  crs = 4326
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
    c(-88.073953680054231, 44.666730975320831),
    crs = 4326
  ) %>%
  mutate(query_fc_type = "point")


multipt_query <-
  sf_point(
    c(-88.104697943398506, 44.584566725366685),
    c(-88.074679844666946, 44.666648232429175),
    crs = 4326
  ) %>%
  mutate(query_fc_type = "multipoint")

# line query layer
line_query <-
  sf_line(c(-88.104697943398506, 44.564566725366685),
          c(-88.104731239830031, 44.594539885604002 ),
          crs = 4326) %>%
  mutate(query_fc_type = "line")



# poly and envelope query layers
poly_query <- sf_polygon(
  c(-88.1,44.7),
  c(-88.238154881955865,44.7),
  c(-88.238154881955865,44.1),
  c(-88.1,44.1),
  c(-88.1,44.7),
  crs = 4326
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

pt_query <- sf_point(c(-88.078713819647362, 44.446311608498327),
                     crs = 4326) %>%
  mutate(query_fc_type = "point")

multipt_query <-
  sf_points(
    c(-87.89001524318387,43.06777561668325),
    c(-88.078713819647362, 44.446311608498327),
    crs = 4326,
  ) %>%
  mutate(query_fc_type = "multipoint")

line_query <-
  sf_line(   c(-87.89001524318387,43.06777561668325),
              c(-88.078713819647362, 44.446311608498327),
              crs = 4326) %>%
  mutate(query_fc_type = "line")

# poly and envelope query layers
poly_query <- sf_polygon(
  c(-88.1,44.7),
  c(-88.238154881955865,44.7),
  c(-88.238154881955865,44.1),
  c(-88.1,44.1),
  c(-88.1,44.7),
  crs = 4326
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
    "brown_cty_hydrants_biketrails_parks_test_results.RData",
    sep = "/"
  )
)
