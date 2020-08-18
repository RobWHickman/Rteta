laggy_func <- function(vec, lags, name) {
  x <- lapply(0:lags, lag, x = vec) %>% do.call(cbind, .) %>% as.data.frame()
  names(x) <- paste0(name, 0:lags)
  return(x)
}
