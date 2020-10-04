context("Formatting Coordinates")
library(arcgis.rest)

#create spatial and character objects to run tests on
#create point object to test
formatted_point <- cbind(0,1)%>%
  sf::st_point() %>%
  sf::st_sfc() %>%
  sf::st_sf(crs = 4326)

formatted_point_char <- "TEST"

#create multipoint object to test
formatted_multipoint <- rbind(c(0, 0), c(1, 1), c(2, 1)) %>%
  sf::st_multipoint() %>%
  sf::st_sfc() %>%
  sf::st_sf(crs = 4326)

formatted_multipoint_char <-
  paste("{'points':[[[0,0],[1,1],[2,1]]],",
        "'spatialReference':{'wkid':4326}}",
        sep = "")

#create line object to test
formatted_line <- rbind(c(0, 0), c(1, 1), c(2, 1)) %>%
  sf::st_linestring() %>%
  sf::st_sfc() %>%
  sf::st_sf(crs = 4326)

formatted_line_char <-
  paste("{'paths':[[[0,0],[1,1],[2,1]]],",
        "'spatialReference':{'wkid':4326}}",
        sep = "")

#create polygon object to test
formatted_poly <- list(rbind(c(0, 0), c(0, 1), c(1, 1), c(1, 0), c(0, 0))) %>%
  sf::st_polygon() %>%
  sf::st_sfc() %>%
  sf::st_sf(crs = 4326)

formatted_poly_char <-
  paste("{'rings':[[[0,0],[0,1],[1,1],[1,0],[0,0]]],",
        "'spatialReference':{'wkid':4326}}",
        sep = "")

#Run Tests
#test format_point_coords
# test_that("format_point_coords returns a properly formatted character object",
#           {
#             test_formatted_point <- format_point_coords(formatted_point)
#             expect_equal(formatted_point_char, test_formatted_point)
#           })

#test format_multipoint_coords
test_that("format_multipoint_coords returns a properly formatted character object",
          {
            test_formatted_multipoint <-
              format_multipoint_coords(formatted_multipoint)
            expect_equal(formatted_multipoint_char, test_formatted_multipoint)
          })

#test format_line_coords
test_that("format_line_coords returns a properly formatted character object",
          {
            test_formatted_line <- format_line_coords(formatted_line)
            expect_equal(formatted_line_char, test_formatted_line)
          })

#test format_polygon_coords
test_that("format_polygon_coords returns a properly formatted character object",
          {
            test_formatted_poly <- format_polygon_coords(mke_county)
            expect_equal(formatted_poly_char, test_formatted_poly)
            })
