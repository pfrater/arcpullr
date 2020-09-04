wi_poly <-
  maps::map("state", regions = "wisconsin",
            plot = FALSE, fill = TRUE, bg = NA) %>%
  sf::st_as_sf()
wi_counties <-
  maps::map("county", regions = "wisconsin",
            plot = FALSE, fill = TRUE, bg = NA) %>%
  sf::st_as_sf() %>%
  tidyr::separate(ID, into = c("state", "county"), sep = ",")
mke_county <-
  wi_counties %>%
  dplyr::filter(county == "milwaukee")
portage_county <-
  wi_counties %>%
  dplyr::filter(county == "portage")


usethis::use_data(wi_poly, wi_counties, mke_county, portage_county,
                  overwrite = TRUE)
