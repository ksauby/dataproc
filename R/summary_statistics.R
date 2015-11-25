#' Return the maximum value of a vector, after removing NAs.
#' @param x Vectors of data.
#' @description Written for use in the tables::tabular function to create publication-ready tables.
#' @export

Maximum <- function(x) base::max(x, na.rm=TRUE)

#' Return the mean of a vector with up to two decimal places, after removing NAs.
#' @param x Vectors of data.
#' @description Written for use in the tables::tabular function to create publication-ready tables.
#' @export

Mean <- function(x) base::mean(x, na.rm=TRUE) %>% round(2)

#' Return the minimum value of a vector, after removing NAs.
#' @param x Vectors of data.
#' @description Written for use in the tables::tabular function to create publication-ready tables.
#' @export

Minimum <- function(x) {min(x, na.rm=TRUE)}

#' Return NA for vector of NAs
#' @param x Vectors of data.
#' @references found here on Nov. 19, 2014: http://tolstoy.newcastle.edu.au/R/help/02a/3218.html
#' @export

mysum <- function(x) {if (all(is.na(x))) NA else sum(x,na.rm=T)} 

#' Return NA for sums below 1
#' @param x Vector of data.
#' @references inspired by: http://tolstoy.newcastle.edu.au/R/help/02a/3218.html
#' @export

mysum1 <- function(x) {if (sum(x,na.rm=T) > 0) 1 else NA}

#' Return 1 for sums greater than 0.
#' @param x Vector of data.
#' @references inspired by: http://tolstoy.newcastle.edu.au/R/help/02a/3218.html
#' @export

mysum2 <- function(x) {if (all(is.na(x))) NA else if (sum(x,na.rm=T) > 0) 1 else 0}

#' Return the sum of a vector, after removing NAs.
#' @param x Vectors of data.
#' @description Written for use in the tables::tabular function to create publication-ready tables.
#' @export

Sum <- function(x) {sum(x, na.rm=TRUE)}
