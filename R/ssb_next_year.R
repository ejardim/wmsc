#' ssb_next_year calculates the survivors at the beginning of the year after your last year in the assessment
#'
#' Produces an FLStock that can be used to plot the SSB in the year after your final year. To be used only if harvest.spwn and m.spwn are 0. Note that in the stock summary plot the recruitment assumption will also be shown.
#'
#' @param stock An object of class FLStock with the assessment results (no default)
#' @param rec.years Number of years to use for the geometric mean recruitment assumption (default: 10)
#' @param biol.years Number of years to use for the mean of the biological parameters (m, stock.wt, mat) (default: 3)
#' @return FLStock. 
#' @export
#' @examples
#' data("ple4")
#' ple4_new <-ssb_next_year(ple4)
#' plot(ple4_new)
#' 
#' ple4_new1 <-ssb_next_year(ple4,3,1)
#' plot(ple4_new1)
 

### function to calculate the SSB at the beginning of the next year

ssb_next_year <- function(stock,rec.years=10,biol.years=3) {
  max_year <- range(stock)["maxyear"]
  stock <-window(stock, end=max_year+1)
  harvest.spwn(stock)[,ac(max_year+1)] <- harvest.spwn(stock)[,ac(max_year)]
  m.spwn(stock)[,ac(max_year+1)] <- m.spwn(stock)[,ac(max_year)] 
  mat(stock)[,ac(max_year+1)]<- yearMeans(mat(stock)[,ac((max_year-biol.years+1):max_year)])
  m(stock)[,ac(max_year+1)]<- yearMeans(m(stock)[,ac((max_year-biol.years+1):max_year)])
  stock.wt(stock)[,ac(max_year+1)]<- yearMeans(stock.wt(stock)[,ac((max_year-biol.years+1):(max_year))])
  stock.n(stock)[,ac(max_year+1)]<- survivors(stock[,ac(max_year)])
  rec(stock)[,ac(max_year+1)] <- exp(mean(log(rec(stock)[,ac((max_year-rec.years+1):(max_year))])))
  return(stock)
}

