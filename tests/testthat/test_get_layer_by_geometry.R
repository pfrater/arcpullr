context("Retrieve Layers by Geometry")

skip_on_cran()

#create a point object
point <- c(-88.526848,45.435216)%>%
  sf::st_point()%>%
  sf::st_sfc() %>%
  sf::st_sf(crs = 4326)

#create a multipoint object
multipoint <- rbind(
c(-88.526848,45.435216),
c(-88.490821,45.444325))%>%
  sf::st_multipoint()%>%
  sf::st_sfc() %>%
  sf::st_sf(crs = 4326)

#create a line object
line <- rbind(c(-88.534444, 45.436479),
              c(-88.521956, 45.433680)) %>%
  sf::st_linestring() %>%
  sf::st_sfc() %>%
  sf::st_sf(crs = 4326)

#create a polygon object
poly <- list(rbind(
  c(-88.533220,45.431675),
  c(-88.532662,45.438511),
  c( -88.519444,45.439143),
  c(-88.519787,45.432036),
  c(-88.533220,45.431675)
)) %>%
  sf::st_polygon() %>%
  sf::st_sfc() %>%
  sf::st_sf(crs = 4326)

hydro_lakes <- paste("https://dnrmaps.wi.gov/arcgis2/rest/services/",
                     "TS_AGOL_STAGING_SERVICES/EN_AGOL_STAGING_SurfaceWater_WTM/",
                     "MapServer/1",
                     sep = "")

#test get_layer_by_point
test_that("get_layer_by_point returns the correct WATERBODY_WBIC",
          {
            otter_lake_point <- get_layer_by_point(url = hydro_lakes,
                                                 geometry = point)
            expect_equal(otter_lake_point$WATERBODY_WBIC,549400)
          })

#test get_layer_by_multipoint

test_that("get_layer_by_multipoint returns a warning",
          {
            expect_warning(
              get_layer_by_multipoint(url = hydro_lakes, geometry = multipoint)
            )
          })

test_that("get_layer_by_multipoint returns the correct WATERBODY_WBIC",
          {
            otter_lake_multipoint <-
              suppressWarnings(
                get_layer_by_multipoint(
                  url = hydro_lakes, geometry = multipoint
                )
              )
            expect_equal(otter_lake_multipoint$WATERBODY_WBIC[1],549400)
          })


#test get_layer_by_line
test_that("get_layer_by_line returns the correct WATERBODY_WBIC",
          {
            otter_lake_line <- get_layer_by_line(url = hydro_lakes,
                                                 geometry =line)
            expect_equal(otter_lake_line$WATERBODY_WBIC,549400)
          })

#test get_layer_by_poly
test_that("get_layer_by_poly returns the correct WATERBODY_WBIC",
          {
            otter_lake_poly <- get_layer_by_poly(url = hydro_lakes,
                                            geometry = poly)
            expect_equal(otter_lake_poly$WATERBODY_WBIC,549400)
          })

#test get_layer_by_envelope
test_that("get_layer_by_envelope returns the correct WATERBODY_WBIC",
          {
            otter_lake_env <- get_layer_by_envelope(url = hydro_lakes,
                                            geometry = poly)
            expect_equal(otter_lake_env$WATERBODY_WBIC,549400)
          })
