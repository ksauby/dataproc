#' Round Function
#' 
#' @description Round multiple columns in a dataframe
#' @param df Dataframe
#' @param digits Number of units to round to
#'
#' @export

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))

  df[,nums] <- round(df[,nums], digits = digits)

  (df)
}