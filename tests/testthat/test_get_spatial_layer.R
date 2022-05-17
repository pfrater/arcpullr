context("Retrieving layers")

skip_on_cran()

stream_hydro_url <- paste0(
  "https://dnrmaps.wi.gov/arcgis2/rest/services/",
  "TS_AGOL_STAGING_SERVICES/EN_AGOL_STAGING_SurfaceWater_WTM/MapServer/2"
)

stream_query_url <- paste(stream_hydro_url, "query", sep = "/")

oak_creek_where <- "RIVER_SYS_NAME = 'Oak Creek'"

# these IDs changed at some point. They must have gotten updated during a
# DNR DB upgrade or something. Removing tests - PNF - 12/17/2021

# oak_creek_ids <- c(
#   909165,  915195, 935421, 937647, 941536, 948799, 953299, 955174,
#   955291, 971135, 973336,  974923,  976365, 1163949, 1173335
# )

# test_that("get_object_ids returns proper IDs", {
#   test_ids <- get_object_ids(
#       query_url = stream_query_url,
#       where = oak_creek_where
#     )
#   expect_equal(sort(test_ids), sort(oak_creek_ids))
# })
#
# test_that("get_esri_features_by_id returns proper feature", {
#   test_feature <- get_esri_features_by_id(
#     ids = oak_creek_ids[1],
#     query_url = stream_query_url,
#     fields = "*"
#   )
#   expect_equal(test_feature[[1]]$attributes$RIVER_SYS_NAME, "Oak Creek")
#   expect_equal(test_feature[[1]]$attributes$RIVER_SYS_WBIC, 14500)
# })

test_that("get_esri_features returns proper feature", {
  test_feature <- get_esri_features(
    query_url = stream_query_url,
    fields = "*",
    where = oak_creek_where,
    head = FALSE
  )
  expect_equal(test_feature[[1]]$attributes$RIVER_SYS_NAME, "Oak Creek")
  expect_equal(test_feature[[1]]$attributes$RIVER_SYS_WBIC, 14500)
})

test_that("get_spatial_layer pulls layer properly", {
  oak_creek <- get_spatial_layer(
      stream_hydro_url,
      where = oak_creek_where
    )
  expect_equal(unique(oak_creek$RIVER_SYS_NAME), "Oak Creek")
  expect_equal(unique(oak_creek$RIVER_SYS_WBIC), 14500)
})
