#' partial_f calculates fishing mortality by unit (fleet)
#'
#' Produces a FLQuant with fishing mortalities by unit.
#'
#' @param stock An object of class FLStock with the assessment results (no default)
#' @param catch_per_fleet catch numbers at age and year per fleet, using the unit dimension
#' @return FLQuant
#' @export
#' @examples

### function to calculate partial fishing mortality by fleet

partial_f <- function(stock, catch_per_fleet, ...) {

  spread(list(...))
  dc <- dim(catch_per_fleet)
  stk <- stock[,,rep(1, dc[3]), rep(1, dc[4]), rep(1, dc[5])]
  pf <- catch_per_fleet/catch.n(stk)*harvest(stk)
  pf

}

