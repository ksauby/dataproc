#' Return the sample variance of a vector, after removing NAs, and round.
#'
#' @param x Vectors of data.
#' @param round_n Number of digits to round to.
#' @description Written for use in the tables::tabular function to create publication-ready tables.
#' @export

Variance <- function(x, round_n=3) stats::var(x, na.rm=TRUE) %>% round(round_n)

#' Return the population variance of a vector, after removing NAs, and round.
#'
#' @param x Vectors of data.
#' @param round_n Number of digits to round to.
#' @description Written for use in the tables::tabular function to create publication-ready tables.
#' @export

PopVariance <- function(x, round_n=3) {
	temp <- sum((x-mean(x))^2)/length(x)
	round(temp, round_n)
}