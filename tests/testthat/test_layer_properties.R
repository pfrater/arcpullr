context("Retrieve Legend")

legend <- get_layer_legend(wi_landcover_url)

legend_colors <- c(
  "#C94242", "#D89382", "#F2EF87", "#E6E2A1", "#E6AC2B", "#9D7207", "#006300",
  "#69B869", "#199919", "#CEECF9", "#95EEF2", "#6FA3BA", "#00D1DD", "#077C82",
  "#797979", "#A8976B"
)

legend_values <- c(
  "Developed, High Intensity ", "Developed, Low Intensity", "Crop Rotation",
  "Cranberries", "Forage Grassland", "Idle Grassland", "Coniferous Forest",
  "Broad-leaved Deciduous Forest", "Mixed Deciduous or Coniferous Forest",
  "Open Water", "Floating Aquatic Herbaceous Vegetation",
  "Emergent or Wet Meadow", "Lowland Scrub or Shrub", "Forested Wetland",
  "Barren", "Shrubland"
)

test_that("legend values and colors are correct", {
  expect_equal(legend$color, legend_colors)
  expect_equal(legend$value, legend_values)
})
