#' Replace 0 with "No".
#' @param x Vector of data.
#' @export

Replace_0_w_No_Function <- function(x){	
	x[which(x == 0)] <- "No"
	return(x)
}

#' Replace 1 with "Yes".
#' @param x Vector of data.
#' @export

Replace_1_w_Yes_Function <- function(x){	
	x[which(x == 1)] <- "Yes"
	return(x)
}

#' Replace NA with a ".".
#' @param x Vector of data.
#' @description Replace NAs with no text so they are not printed on datasheets.
#' @export

Replace_NA_w_Period_Function <- function(x){
	x[is.na(x) == "TRUE"] <- ""
	return(x)
}

#' Replace 0 with NA.
#' @param x Vector of data.
#' @export

Zero_is_NA_Function <- function(x){	
	x[which(x == 0)] <- NA
	return(x)
}

#' Standardize "Yes" values.
#' @param x Vector of data.
#' @export

Yes_Function <- function(x){	
	x[which(
		x == "Yes" | 
		x == "yes" |
		x == "y" |
		x == "Egg" | 
		x == "Larva" | 
		x == "Nymph"
	)] <- "1"
	return(x)
}

#' Replace values with NA.
#' @param x Vector of data.
#' @examples
#' x = c(1,2,3,4," ", "Not Recorded")
#' NA_Function(x)
#' @export

NA_Function <- function(x){	
	x[which(
		x == "Not Recorded" | 
		x == "not recorded" | 
		x == "Not recorded" | 
		x == "unknown" | 
		x == "Unknown" | 
		x == Inf |
		x == -Inf | 
		x == "NaN" | 
		x == NaN | 
		x == 999 | 
		x == 9999 |
		x == -9999 |
		x == "-999.9" |
		x == "9999" |
		x == " "
	)] <- NA
	return(x)
}

#' Change NA values to 0.
#' @param x Vector of data.
#' @export

NA_is_Zero_Function <- function(x){	
	x[which(is.na(x))] <- 0
	return(x)
}

#' Change "No" values to 0.
#' @param x Vector of data.
#' @export

No_Function <- function(x){	
	x[which(
		x == "None" | 
		x == "No" | 
		x == "no" | 
		x == "none" | 
		x == "Feeding evidence, but no insects"
	)] <- "0"
	return(x)
}