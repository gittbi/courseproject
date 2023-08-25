#' meantable()
#'
#' @param x 
#' @param colvar 
#' @param rowvar 
#' @param statvar 
#'
#' @return a table
#' @export
#'
#' @examples meantable(fl, origin, carrier, air_time)
#' 
#' @importFrom reshape2 acast
#' @import dplyr
meantable <- function(x, colvar, rowvar, statvar) {
  df <- x %>% 
    group_by({{colvar}}, {{rowvar}}) %>% 
    summarise(avg_statvar = mean({{statvar}}, na.rm = TRUE), .groups = "drop")
  # df
  array <- acast(df, df[[1]] ~ df[[2]], value.var = "avg_statvar")
  as.table(array)
}