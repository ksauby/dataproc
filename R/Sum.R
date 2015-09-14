#' Return the sum of a vector, after removing NAs.
#' @param x Vectors of data.
#' @description Written for use in the tables::tabular function to create publication-ready tables.
Sum <- function(x) base::sum(x, na.rm=TRUE)
