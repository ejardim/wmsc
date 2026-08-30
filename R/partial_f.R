#' partial_f calculates fishing mortality by fleet
#'
#' Produces a FLStocks with the harvest slot update
#'
#' @param fit An object of class a4aFitSA with the assessment fit (no default)
#' @param partial_stocks An object of class FLStocks containing the partial stock objects (fleets)
#' @return FLStocks
#' @export


### function to calculate partial fishing mortality by fleet

partial_f <- function(fit, partial_stocks, ...) {
  
  spread(list(...))
  
  f_tot <- harvest(fit)
  c_fleets <- lapply(partial_stocks, catch.n)
  c_tot <- Reduce("+", c_fleets)
  pf <- lapply(c_fleets, function(x) {
    (x / c_tot) * f_tot
  })

 upd_stks <- FLStocks(Map(function(x, y) {
  harvest(x) <- y  
  return(x) 
  }, partial_stocks, pf))
  return(upd_stks)
}
