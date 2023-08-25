#' Title
#'
#' @param x 
#' @param colvar 
#' @param rowvar 
#' @param statvar 
#'
#' @return
#' @export
#'
#' @examples
meantable <- function(x, colvar, rowvar, statvar) {
  df <- x %>% 
    group_by({{colvar}}, {{rowvar}}) %>% 
    summarise(avg_statvar = mean({{statvar}}, na.rm = TRUE), .groups = "drop")
  # df
  df2 <- reshape2::dcast(df, df[[1]] ~ df[[2]], value.var = "avg_statvar")
  return = df2
}