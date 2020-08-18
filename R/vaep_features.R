x <- lapply(0:n, lag, x = spadl_events$type_id) %>% do.call(cbind, .) %>% as.data.frame()
names(x) <- paste0("type_id_a", seq(ncol(x)))


laggy_func <- function(vec, lags, name) {
  x <- lapply(0:lags, lag, x = vec) %>% do.call(cbind, .) %>% as.data.frame()
  names(x) <- paste0(name, 0:lags)
  return(x)
}
FIX DUEL + LOST IN PLAY - SUCESS NOT FAILURE
