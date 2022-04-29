context("Utilities functions")

pt_a <- c(-90, 45)
pt_b <- c(-89, 44)
sf_pt <- sf_points(pt_a)
sf_multipt <- sf_points(pt_a, pt_b)
sf_line <- sf_line(pt_a, pt_b)
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

test_that("get_sf_crs returns the correct CRS values for polygons", {
  expect_equal(get_sf_crs(iceland_poly), 4326)
  expect_equal(get_sf_crs(mke_county), 4326)
})

test_that("sp_rel_lookup returns correct spatial relation character string", {
  sp_xref <- sp_rel_ref$sp.xref
  sp_xref2 <- gsub("e?s$", "", sp_xref)
  expect_equal(sp_rel_xref(sp_xref[1]), "esriSpatialRelContains")
  expect_equal(sp_rel_xref(sp_xref2[1]), "esriSpatialRelContains")
  expect_equal(sp_rel_xref("esriSpatialRelContains"), "esriSpatialRelContains")

  expect_equal(sp_rel_xref(sp_xref[2]), "esriSpatialRelCrosses")
  expect_equal(sp_rel_xref(sp_xref2[2]), "esriSpatialRelCrosses")
  expect_equal(sp_rel_xref("esriSpatialRelCrosses"), "esriSpatialRelCrosses")

  expect_equal(sp_rel_xref(sp_xref[3]), "esriSpatialRelEnvelopeIntersects")
  expect_equal(sp_rel_xref(sp_xref2[3]), "esriSpatialRelEnvelopeIntersects")
  expect_equal(
    sp_rel_xref("esriSpatialRelEnvelopeIntersects"),
    "esriSpatialRelEnvelopeIntersects"
  )

  expect_equal(sp_rel_xref(sp_xref[4]), "esriSpatialRelIndexIntersects")
  expect_equal(sp_rel_xref(sp_xref2[4]), "esriSpatialRelIndexIntersects")
  expect_equal(
    sp_rel_xref("esriSpatialRelIndexIntersects"),
    "esriSpatialRelIndexIntersects"
  )

  expect_equal(sp_rel_xref(sp_xref[5]), "esriSpatialRelIntersects")
  expect_equal(sp_rel_xref(sp_xref2[5]), "esriSpatialRelIntersects")
  expect_equal(
    sp_rel_xref("esriSpatialRelIntersects"),
    "esriSpatialRelIntersects"
  )

  expect_equal(sp_rel_xref(sp_xref[6]), "esriSpatialRelOverlaps")
  expect_equal(sp_rel_xref(sp_xref2[6]), "esriSpatialRelOverlaps")
  expect_equal(sp_rel_xref("esriSpatialRelOverlaps"), "esriSpatialRelOverlaps")

  expect_equal(sp_rel_xref(sp_xref[7]), "esriSpatialRelRelation")
  expect_equal(sp_rel_xref(sp_xref2[7]), "esriSpatialRelRelation")
  expect_equal(sp_rel_xref("esriSpatialRelRelation"), "esriSpatialRelRelation")

  expect_equal(sp_rel_xref(sp_xref[8]), "esriSpatialRelTouches")
  expect_equal(sp_rel_xref(sp_xref2[8]), "esriSpatialRelTouches")
  expect_equal(sp_rel_xref("esriSpatialRelTouches"), "esriSpatialRelTouches")

  expect_equal(sp_rel_xref(sp_xref[9]), "esriSpatialRelWithin")
  expect_equal(sp_rel_xref(sp_xref2[9]), "esriSpatialRelWithin")
  expect_equal(sp_rel_xref("esriSpatialRelWithin"), "esriSpatialRelWithin")

  expect_error(sp_rel_xref("test"))
})
