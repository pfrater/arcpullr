library(sf)
library(tidyverse)

#points v points
url <- "https://dnrmaps.wi.gov/arcgis/rest/services/FM_Trout/FM_TROUT_HAB_SITES_WTM_Ext/MapServer/0"
sf_query <- get_spatial_layer(url)
sf_query <- st_transform(sf_query,crs = 3071)
sf_query <- slice(sf_query,1)

test1 <- tibble(service_url = url,
                query_object = list(sf_query),
                service_geom = "multipoint",
                query_geom = "point",
                sp_rel = sp_rel_lookup$sp_rel) %>%
  mutate(
    service_object =
      map(
        .x = sp_rel,
        .f =
          ~ get_layer_by_point(
            sp_rel = .x,
            url = url,
            geometry = sf_query
          )
      ),
    valid = map_int(
      .x = service_object,
      .f =
        ~ ncol(tibble(.x))
      ),
    valid = if_else(valid > 1,TRUE, FALSE)
  )

#line v point
url <- "https://dnrmaps.wi.gov/arcgis/rest/services/DW_Map_Cached/EN_Basic_Basemap_WTM_Ext/MapServer/4"
sf_query <-
  arcpullr::sf_point(
    c(686090.89600249426, 286395.55429548677),
    crs = 3071
  )

test2 <- tibble(
  service_url = url,
  query_object = list(sf_query),
  service_geom = "line",
  query_geom = "point",
  sp_rel = sp_rel_lookup$sp_rel
) %>%
  mutate(
    service_object =
      map(
        .x = sp_rel,
        .f =
          ~ get_layer_by_point(
            service_url = url,
            query_object = list(sf_query),
            sp_rel = .x,
            url = url,
            geometry = sf_query
          )
      ),
    valid = map_int(.x = service_object,
                    .f =
                      ~ ncol(tibble(.x))),
    valid = if_else(valid > 1, TRUE, FALSE)
  )

#polygon v point
sf_query <- arcpullr::sf_point(c(-88.5274, 45.43536))
url <- "https://dnrmaps.wi.gov/arcgis2/rest/services/TS_AGOL_STAGING_SERVICES/EN_AGOL_STAGING_SurfaceWater_WTM/MapServer/1"

test3 <- tibble(
  service_url = url,
  query_object = list(sf_query),
  service_geom = "polygon",
  query_geom = "point",
  sp_rel = sp_rel_lookup$sp_rel) %>%
  mutate(
    service_object =
      map(
        .x = sp_rel,
        .f =
          ~ get_layer_by_point(
            sp_rel = .x,
            url = url,
            geometry = sf_query
          )
      ),
    valid = map_int(
      .x = service_object,
      .f =
        ~ ncol(tibble(.x))
    ),
    valid = if_else(valid > 1,TRUE, FALSE)
  )


# Test get_layer_by_multipoint --------------------------------------------
#multipoint v multipoint
url <- "https://dnrmaps.wi.gov/arcgis/rest/services/FM_Trout/FM_TROUT_HAB_SITES_WTM_Ext/MapServer/0"
sf_query <- get_spatial_layer(url)
sf_query <- st_transform(sf_query,crs = 3071)
sf_query <- slice(sf_query,1:5)

test4 <- tibble(
  service_url = url,
  query_object = list(sf_query),
  service_geom = "multipoint",
                query_geom = "multipoint",
                sp_rel = sp_rel_lookup$sp_rel) %>%
  mutate(
    service_object =
      map(.x = sp_rel,
          .f =
            ~ get_layer_by_multipoint(
              sp_rel = .x,
              url = url,
              geometry = sf_query
          )
      ),
    valid = map_int(
      .x = service_object,
      .f =
        ~ ncol(tibble(.x))
    ),
    valid = if_else(valid > 1,TRUE, FALSE)
  )

#point v line
sf_query <-
  arcpullr::sf_points(
    c(679812.95480241114, 287892.79189548083),
    c(686090.89600249426, 286395.55429548677),
    crs = 3071
  )
url <- "https://dnrmaps.wi.gov/arcgis/rest/services/DW_Map_Cached/EN_Basic_Basemap_WTM_Ext/MapServer/4"

test5 <- tibble(
  service_url = url,
  query_object = list(sf_query),
  service_geom = "line",
  query_geom = "multipoint",
  sp_rel = sp_rel_lookup$sp_rel) %>%
  mutate(
    service_object =
      map(.x = sp_rel,
          .f =
            ~ get_layer_by_multipoint(
              sp_rel = .x,
              url = url,
              geometry = sf_query
            )
      ),
    valid = map_int(
      .x = service_object,
      .f =
        ~ ncol(tibble(.x))
    ),
    valid = if_else(valid > 1,TRUE, FALSE)
  )

#polygon v multipoint
sf_query <- arcpullr::sf_points(
  c(-88.52849, 45.43534),
  c(-88.49072, 45.44492))
url <- "https://dnrmaps.wi.gov/arcgis2/rest/services/TS_AGOL_STAGING_SERVICES/EN_AGOL_STAGING_SurfaceWater_WTM/MapServer/1"

test6 <- tibble(
  service_url = url,
  query_object = list(sf_query),
  service_geom = "polygon",
  query_geom = "multipoint",
  sp_rel = sp_rel_lookup$sp_rel) %>%
  mutate(
    service_object =
      map(
        .x = sp_rel,
        .f =
          ~ get_layer_by_multipoint(
            sp_rel = .x,
            url = url,
            geometry = sf_query
          )
      ),
    valid = map_int(.x = service_object,
                    .f =
                      ~ ncol(tibble(.x))),
    valid = if_else(valid > 1, TRUE, FALSE)
  )

sp_rel_tests <- rbind(test1,test2,test3,test4,test5,test6)
rm(test1,test2,test3,test4,test5,test6,sf_query,url)

