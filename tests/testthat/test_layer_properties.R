context("Retrieve Legend")

legend <- get_layer_legend(wi_landcover_url)

legend_colors <- c(
  "#C84141", "#D89382", "#F2EE86", "#E6E2A1", "#E6AC2E", "#9C7209",
  "#016400", "#6AB86A", "#1E981E", "#CEEBF9", "#95EDF2", "#70A3BA",
  "#00D1DC", "#0F7C82", "#787878", "#A8976D"
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
