library(tidyverse)

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
  select(fc_type, query_fc_type, sp_rel)
