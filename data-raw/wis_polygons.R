devtools::load_all(".")

wis_poly <-
  maps::map("state", regions = "wisconsin",
            plot = FALSE, fill = TRUE, bg = NA) |>
  sf::st_as_sf()

wis_counties <-
  maps::map("county", regions = "wisconsin",
            plot = FALSE, fill = TRUE, bg = NA) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4326) |>
  tidyr::separate(ID, into = c("state", "county"), sep = ",") |>
  dplyr::arrange(county) |>
  dplyr::mutate(county = tolower(gsub(" ", "_", county)), county.code = 1:72) |>
  dplyr::relocate(county.code, .after = county)

mke_county <-
  wis_counties |>
  dplyr::filter(county == "milwaukee")
portage_county <-
  wis_counties |>
  dplyr::filter(county == "portage")


usethis::use_data(
  wis_poly,
  wis_counties,
  mke_county,
  portage_county,
  overwrite = TRUE
)
