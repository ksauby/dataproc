#' Ifelse function that retains the original class of a variable
#' 
#' @param cond Condition
#' @param yes action if variable meets condition
#' @param no action if variable does not meet condition
#' @description Retain the class of a variable when using ifelse. Copied from http://stackoverflow.com/questions/6668963/how-to-prevent-ifelse-from-turning-date-objects-into-numeric-objects.
#'
#' @export

safe.ifelse <- function(cond, yes, no) {
	structure(
		ifelse(cond, yes, no), 
		class = class(yes)
	)
}

#' Return the maximum value of a vector, after removing NAs.
#' 
#' @param x Vectors of data.
#' @description Written for use in the tables::tabular function to create publication-ready tables.
#'
#' @export

Maximum <- function(x) {
	ifelse(
		!all(is.na(x)),
		max(x, na.rm=TRUE),
		NA
	)
}

#' Return the mean of a vector with up to two decimal places, after removing NAs.
#' 
#' @param x Vectors of data.
#' @description Written for use in the tables::tabular function to create publication-ready tables.
#'
#' @export

Mean <- function(x) base::mean(x, na.rm=TRUE)

#' Return the percentage of a vector with up to one decimal places, after removing NAs.
#' 
#' @param x Vectors of data.
#' @description Written for use in the tables::tabular function to create publication-ready tables.
#'
#' @export

Percentage <- function(x) base::mean(x, na.rm=TRUE) %>% round(3)*100

#' Return the minimum value of a vector, after removing NAs.
#' 
#' @param x Vectors of data.
#' @description Written for use in the tables::tabular function to create publication-ready tables.
#'
#' @export

Minimum <- function(x) {min(x, na.rm=TRUE)}

#' Return NA for vector of NAs
#' 
#' @param x Vectors of data.
#' @references found here on Nov. 19, 2014: http://tolstoy.newcastle.edu.au/R/help/02a/3218.html
#'
#' @export

mysum <- function(x) {if (all(is.na(x))) NA else sum(x,na.rm=T)} 

#' Return NA for sums below 1
#' 
#' @param x Vector of data.
#' @references inspired by: http://tolstoy.newcastle.edu.au/R/help/02a/3218.html
#'
#' @export

mysum1 <- function(x) {if (sum(x,na.rm=T) > 0) 1 else NA}

#' Return 1 for sums greater than 0.
#' 
#' @param x Vector of data.
#' @references inspired by: http://tolstoy.newcastle.edu.au/R/help/02a/3218.html
#'
#' @export

mysum2 <- function(x) {
	if (all(is.na(x))) NA else 
		if (sum(x,na.rm=T) > 0) 1 else 
			0
}

#' Return 1 for sums greater than 0, as well as for when vector is all NA.
#' 
#' @param x Vector of data.
#' @references inspired by: http://tolstoy.newcastle.edu.au/R/help/02a/3218.html
#'
#' @export

mysum3 <- function(x) {if (sum(x,na.rm=T) > 0) 1 else 0}

#' Return the sum of a vector, after removing NAs.
#' 
#' @param x Vectors of data.
#' @description Written for use in the tables::tabular function to create publication-ready tables.
#'
#' @export

Sum <- function(x) {sum(x, na.rm=TRUE)}

#' Population Coefficient of Variation
#' 
#' @param x Vectors of data.
#'
#' @export

popCV <- function(x) {sqrt(PopVariance(x))/Mean(x)}

#' Sample Coefficient of Variation
#' 
#' @param x Vectors of data.
#'
#' @export

sampleCV <- function(x) {sqrt(var(x, na.rm=T))/Mean(x)}

