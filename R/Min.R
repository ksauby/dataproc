#' Return the minimum value of a vector, after removing NAs.
#' @param x Vectors of data.
#' @description Written for use in the tables::tabular function to create publication-ready tables.
Minimum <- function(x) base::min(x, na.rm=TRUE)
