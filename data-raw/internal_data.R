sp_rel_ref <-
  data.frame(sp.rel.ref = unique(sp_rel_lookup$sp_rel)) %>%
  dplyr::mutate(sp.xref = tolower(str_remove(sp.rel.ref, "esriSpatialRel")))

usethis::use_data(sp_rel_ref, internal = TRUE, overwrite = TRUE)
