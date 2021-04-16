library(tidyverse)

test_result_names <- c(
  "hydro_trout_hab",
  "goodyear_seawall_outstanding_streams"
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
  test_results[[1]] %>%
  left_join(test_results[[2]], by = c("fc_type", "query_fc_type", "sp_rel"))

check_results <-
  all_results %>%
  filter(hydro_trout_hab_valid != goodyear_seawall_outstanding_streams_valid)

