#' Return the maximum value of a vector, after removing NAs.
#' @param x Vectors of data.
#' @description Written for use in the tables::tabular function to create publication-ready tables.

Max <- function(x) base::max(x, na.rm=TRUE)
