context("Formatting Coordinates")

#create spatial and character objects to run tests on
multipoint <- sf_points(c(0, 0), c(1, 1), c(2, 1))
line <- sf_line(c(0, 0), c(1, 1), c(2, 1))
poly <- sf_polygon(
  c(0, 0), c(0, 1), c(0, 2), c(1, 2), c(1, 3), c(2, 3),
  c(2, 4), c(2, 5), c(1, 5), c(0, 5), c(0, 3), c(0, 0)
)

# make polyline
line2 <- sf_line(c(0, 1), c(1, 2), c(3, 2))
polyline <- rbind(line, line2)

# make multi-polygon
poly2 <- sf_polygon(
  c(10, 10), c(10, 11), c(10, 12), c(11, 12), c(11, 13), c(12, 13),
  c(12, 14), c(12, 15), c(11, 15), c(10, 15), c(10, 13), c(10, 10)
)
multipoly <- rbind(poly, poly2)

#run tests
# test format_multipoint_coords
expected_multipoint_output <- paste0(
  "{'points':[[0,0],[1,1],[2,1]], ",
  "'spatialReference':{'wkid':4326}}"
)

test_that(
  "format_multipoint_coords returns a properly formatted character object", {
    actual_multipoint_output <-
      format_multipoint_coords(multipoint)
    expect_equal(expected_multipoint_output, actual_multipoint_output)
  }
)

# test format_line_coords
expected_line_output <- paste0(
  "{'paths':[[[0,0],[1,1],[2,1]]], ",
  "'spatialReference':{'wkid':4326}}"
)

test_that("format_line_coords returns a properly formatted character object",
          {
            actual_line_output <- format_line_coords(line)
            expect_equal(expected_line_output, actual_line_output)
          })

# test format_polygon_coords
expected_polygon_output <- paste0(
  "{'rings':[[[0,0],[0,1],[0,2],[1,2],[1,3],[2,3],[2,4],[2,5],[1,5],",
  "[0,5],[0,3],[0,0]]], 'spatialReference':{'wkid':4326}}"
)

test_that(
  "format_polygon_coords returns a properly formatted character object", {
    actual_polygon_output <- format_polygon_coords(poly)
    expect_equal(expected_polygon_output, actual_polygon_output)
  }
)

# test sf object with multiple lines
expected_polyline_output <- paste0(
  "{'paths':[[[0,0],[1,1],[2,1]], [[0,1],[1,2],[3,2]]], ",
  "'spatialReference':{'wkid':4326}}"
)

test_that(
  "format_line_coords properly formats multiple lines", {
    actual_polyline_output <- format_line_coords(polyline)
    expect_equal(expected_polyline_output, actual_polyline_output)
  }
)

# test sf object with multiple polygons
expected_multipoly_output <- paste0(
  "{'rings':[[[0,0],[0,1],[0,2],[1,2],[1,3],[2,3],",
  "[2,4],[2,5],[1,5],[0,5],[0,3],[0,0]], ",
  "[[10,10],[10,11],[10,12],[11,12],[11,13],[12,13],",
  "[12,14],[12,15],[11,15],[10,15],[10,13],[10,10]]], ",
  "'spatialReference':{'wkid':4326}}"
)

test_that(
  "format_polygon_coords properly formats multiple polygons", {
    actual_multipoly_output <- format_polygon_coords(multipoly)
    expect_equal(expected_multipoly_output, actual_multipoly_output)
  }
)

# test format_envelope_coords
expected_envelope_output <- "xmin : 0, ymin : 0, xmax : 2, ymax : 5"

test_that(
  "format_envelope_coords returns a properly formatted character object", {
    actual_envelope_output <- format_envelope_coords(poly)
    expect_equal(expected_envelope_output, actual_envelope_output)
  }
)
