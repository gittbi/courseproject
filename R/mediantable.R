#' mediantable()
#'
#' @param x 
#' @param colvar 
#' @param rowvar 
#' @param statvar 
#'
#' @return
#' @export
#'
#' @examples mediantable(fl, origin, carrier, air_time)
#' @importFrom reshape2 acast
#' @import dplyr
mediantable <- function(x, colvar, rowvar, statvar) {
  df <- x %>% 
    group_by({{colvar}}, {{rowvar}}) %>% 
    summarise(median_statvar = median({{statvar}}, na.rm = TRUE), .groups = "drop")
  # df
  array <- acast(df, df[[1]] ~ df[[2]], value.var = "median_statvar")
  as.table(array)
}