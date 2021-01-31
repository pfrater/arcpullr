context("Utilities functions")

pt_a <- c(-90, 45)
pt_b <- c(-89, 44)
sf_pt <- sf_points(pt_a)
sf_multipt <- sf_points(pt_a, pt_b)
sf_line <- sf_lines(pt_a, pt_b)
sf_poly <- sf_polygon(c(0, 0), c(0, 1), c(1, 1), c(1,0), c(0,0))

test_that("sf_objects correctly return sf objects", {
  expect_equal(class(sf_pt), c("sf", "data.frame"))
  expect_equal(class(sf_multipt), c("sf", "data.frame"))
  expect_equal(class(sf_line), c("sf", "data.frame"))
  expect_equal(class(sf_poly), c("sf", "data.frame"))
})

test_that("sql_where returns correct SQL WHERE clauses", {
  expect_equal(sql_where(foo = "bar"), "foo = 'bar'")
  expect_equal(
    sql_where(foo = "bar", bar = "baz"),
    "foo = 'bar' AND bar = 'baz'"
  )
  expect_equal(
    sql_where(foo = c("bar", "baz"), rel_op = "IN"),
    "foo IN ( 'bar' , 'baz' )"
  )
  expect_equal(
    sql_where(foo = c("bar", "baz"), a = "b", rel_op = "IN"),
    "foo IN ( 'bar' , 'baz' ) AND a IN 'b'"
  )
})

test_that("get_geometry_type returns the correct geometry types", {
  server <- "https://dnrmaps.wi.gov/arcgis/rest/services/WT_SWDV/"
  points_service <- "WT_Monitoring_Sites_and_Data_WTM_Ext_v2/MapServer/2"
  line_service <- "WT_Inland_Water_Resources_WTM_Ext_v2/MapServer/2"
  polygon_service <- "WT_Inland_Water_Resources_WTM_Ext_v2/MapServer/3"

  expect_equal(get_geometry_type(paste0(server,points_service)),
               "esriGeometryPoint"
  )
  expect_equal(get_geometry_type(paste0(server,line_service)),
               "esriGeometryPolyline"
               )
  expect_equal(get_geometry_type(paste0(server,polygon_service)),
               "esriGeometryPolygon"
  )
})
