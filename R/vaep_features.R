vaep_features_lag <- function(vec, lags, name) {
  x <- lapply(0:lags, lag, x = vec) %>% do.call(cbind, .) %>% as.data.frame()
  names(x) <- paste0(name, 0:lags)
  return(x)
}

vaep_get_features <- function(spadl) {
  type_ids <- vaep_features_lag(spadl$type_id, 2, "type_id_a")
  bodypart_ids <- vaep_features_lag(spadl$bodypart_id, 2, "bodypart_id_a")
  result_ids <- vaep_features_lag(spadl$result_id, 2, "result_id_a")
}
