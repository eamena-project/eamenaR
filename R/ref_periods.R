#' Basic statistics on GeoJSON file like lists or charts
#'
#' @name ref_periods
#'
#' @description Workout on EAMENA cultural periods
#'
#' @param stat type of statistic that will be computed. Default `order` (ordered/seriated periods on their mean).
#' @param ref.periods the periods reference table.
#' @param name of field on which paths will be grouped. For example "route". Will create as many plots as there are different categories. Default NA.
#' @param verbose if TRUE (by default), print messages.
#'
#' @return Depending on the parameter `stat`. If `stat` = `order` will return a vector of period labels.
#'
#' @examples
#'
#' # return the ordered vector of cultural periods
#' ref_periods()
#'
#' @export
ref_periods <- function(stat = c("order"),
                        ref.periods = "https://raw.githubusercontent.com/achp-project/cultural-heritage/main/periodo-projects/cultural_periods.tsv",
                        verbose = TRUE){
  if("order" %in% stat)
  periods.df <- read.csv2(ref.periods, sep = "\t")
  duration <- abs(periods.df$ea.duration.tpq - periods.df$ea.duration.taq)
  periods.df$mean <- periods.df$ea.duration.taq + duration/2
  periods.df <- periods.df[order(periods.df$mean), ]
  return(periods.df$ea.name)
}
