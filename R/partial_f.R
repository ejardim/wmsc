#' partial_f calculates fishing mortality by fleet
#'
#' Produces a FLQuant with fishing mortalities by unit.
#'
#' @param fit An object of class a4aFitSA with the assessment fit (no default)
#' @param partial_stocks An object of class FLStocks containing the partial stock objects (fleets)
#' @return FLQuants
#' @export


### function to calculate partial fishing mortality by fleet

partial_f <- function(fit, partial_stocks, ...) {
  
  spread(list(...))
  
  f_tot <- harvest(fit)
  c_fleets <- catch.n(partial_stocks)
  c_tot <- Reduce("+", c_fleets)
  pf <- lapply(c_fleets, function(x) {
    (x / c_tot) * f_tot
  })
  return(pf)
}
