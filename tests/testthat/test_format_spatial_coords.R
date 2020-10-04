context("Formatting Coordinates")
library(arcgis.rest)

#create spatial and character objects to run tests on
#create multipoint object to test
multipoint <- rbind(c(0, 0), c(1, 1), c(2, 1)) %>%
  sf::st_multipoint() %>%
  sf::st_sfc() %>%
  sf::st_sf(crs = 4326)
#create line object to test
line <- rbind(c(0, 0), c(1, 1), c(2, 1)) %>%
  sf::st_linestring() %>%
  sf::st_sfc() %>%
  sf::st_sf(crs = 4326)
#create polygon object to test
poly <-
  list(rbind(
    c(0, 0), c(0, 1), c(0, 2), c(1, 2), c(1, 3), c(2, 3),
    c(2, 4), c(2, 5), c(1, 5), c(0, 5), c(0, 3), c(0, 0)
  )) %>%
  sf::st_polygon() %>%
  sf::st_sfc() %>%
  sf::st_sf(crs = 4326)

#run tests
#test format_multipoint_coords
expected_mulitpoint_output <-
  paste("{'points':[[[0,0],[1,1],[2,1]]],",
        "'spatialReference':{'wkid':4326}}",
        sep = "")

test_that("format_multipoint_coords returns a properly formatted character object",
          {
            actual_multipoint_output <-
              format_multipoint_coords(multipoint)
            expect_equal(expected_mulitpoint_output, actual_multipoint_output)
          })

#test format_line_coords
expected_line_output <-
  paste("{'paths':[[[0,0],[1,1],[2,1]]],",
        "'spatialReference':{'wkid':4326}}",
        sep = "")

test_that("format_line_coords returns a properly formatted character object",
          {
            actual_line_output <- format_line_coords(line)
            expect_equal(expected_line_output, actual_line_output)
          })

#test format_polygon_coords
expected_polygon_output <-
  paste("{'rings':[[[0,0],[0,1],[0,2],[1,2],[1,3],[2,3],[2,4],[2,5],[1,5],",
        "[0,5],[0,3],[0,0]]],'spatialReference':{'wkid':4326}}",
        sep = "")

test_that("format_polygon_coords returns a properly formatted character object",
          {
            actual_polygon_output <- format_polygon_coords(poly)
            expect_equal(expected_polygon_output, actual_polygon_output)
            })
#test format_envelope_coords
expected_envelope_output <- "xmin : 0, ymin : 0, xmax : 2, ymax : 5"

test_that("format_envelope_coords returns a properly formatted character object",
          {
            actual_envelope_output <- format_envelope_coords(poly)
            expect_equal(expected_envelope_output, actual_envelope_output)
          })
