#' Return the variance of a vector with up to two decimal places, after removing NAs.
#'
#' @param x Vectors of data.
#' @description Written for use in the tables::tabular function to create publication-ready tables.
#' @export

Variance <- function(x, round_n=3) stats::var(x, na.rm=TRUE) %>% round(round_n)
