
url <- paste(
  "https://desktop.arcgis.com/en/arcmap/latest/extensions/",
  "data-reviewer/types-of-spatial-relationships-that-can-be-validated.htm",
  sep = "")

sp_rel_valid <-
 xml2::read_html(url) %>%
  rvest::html_node("table") %>%
  rvest::html_table() %>%
  dplyr::rename(sp_rel = 3) %>%
  dplyr::mutate(
    sp_rel = base::sub('\\Note.*', '', sp_rel),
    sp_rel = base::gsub('([[:upper:]])', ' \\1', sp_rel),
    sp_rel = stringr::str_remove(sp_rel, "\r\n"),
    sp_rel = stringr::str_remove(sp_rel, "Relation")
  ) %>%
  janitor::clean_names() %>%
  tidyr::separate(sp_rel,
                  into = c("a", "b", "c", "d", "e", "f", "g", "i")) %>%
  tidyr::pivot_longer(!c(feature_class_1, feature_class_2), names_to = "sp_rel") %>%
  dplyr::select(-sp_rel) %>%
  dplyr::rename(sp_rel = value) %>%
  dplyr::arrange(feature_class_1, feature_class_2) %>%
  dplyr::filter(sp_rel != "" & !is.na(sp_rel))

url <- paste(
  "https://help.arcgis.com/en/webapi/wpf/apiref/",
  "ESRI.ArcGIS.Client~ESRI.ArcGIS.Client.Tasks.SpatialRelationship.html",
  sep = "")

sp_rel_lookup <-
  xml2::read_html(url) %>%
  rvest::html_nodes("table")%>%
  magrittr::extract2(6)%>%
  rvest::html_table()

usethis::use_data(sp_rel_lookup, sp_rel_valid, overwrite = TRUE)
