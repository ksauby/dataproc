#' Return the mean of a vector with up to two decimal places, after removing NAs.
#' @param x Vectors of data.
#' @description Written for use in the tables::tabular function to create publication-ready tables.

Mean <- function(x) base::mean(x, na.rm=TRUE) %>% round(2)
