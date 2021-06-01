library(tidyverse)


# creating an sp_rel_valid table based on our tests of what works---------------
test_result_names <- c(
  "hydro_trout_hab",
  "goodyear_seawall_outstanding_streams",
  "brown_cty_hydrants_biketrails_parks"
)

test_results <- lapply(test_result_names, function(x) {
  load(paste0("other_tests/sp_rel_tests/", x, "_test_results.RData"))
  out <-
    test_results %>%
    select(fc_type, query_fc_type, sp_rel, valid)
  names(out)[4] <- paste(x, "valid", sep = "_")
  return(out)
})

all_results <-
  Reduce(function(...) {
    left_join(..., by = c("fc_type", "query_fc_type", "sp_rel"))
  }, test_results)

check_results <-
  all_results %>%
  mutate(test_sum = select(., ends_with("valid")) %>% rowSums()) %>%
  filter(test_sum %in% c(0, 3))

sp_rel_valid <-
  check_results %>%
  filter(test_sum == 3) %>%
  rbind(filter(., fc_type == "point") %>% mutate(fc_type = "multipoint")) %>%
  select(fc_type, query_fc_type, sp_rel) %>%
  arrange(fc_type, query_fc_type, sp_rel) %>%
  rename(feature_class = fc_type,
         query_feature_class = query_fc_type)


# create a lookup table of the descriptions of each sp_rel----------------------
url <- paste(
  "https://help.arcgis.com/en/webapi/wpf/apiref/",
  "ESRI.ArcGIS.Client~ESRI.ArcGIS.Client.Tasks.SpatialRelationship.html",
  sep = "")

sp_rel_lookup <-
  xml2::read_html(url) %>%
  rvest::html_nodes("table")%>%
  magrittr::extract2(6)%>%
  rvest::html_table() %>%
  dplyr::rename(sp_rel = Member, description = Description)

usethis::use_data(sp_rel_lookup, sp_rel_valid, overwrite = TRUE)
