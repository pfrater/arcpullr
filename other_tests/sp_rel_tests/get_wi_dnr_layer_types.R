library(tidyverse)

wdnr_layers <- filter(wdnr.gis::service_urls, !is.na(url))

geom_types <- lapply(1:nrow(wdnr_layers), function(x) {
  dat <- wdnr_layers[x, ]
  out <- tryCatch(get_geometry_type(dat$url), error = function(e) NULL)
  dat$geom_type <- out
  Sys.sleep(0.25)
  return(dat)
})

geom_data <- do.call("rbind", geom_types)

# no multipoint layers on wdnr arcgis rest api
unique(geom_data$geom_types)
