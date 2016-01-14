#' Format dates
#' 
#' @param x Vectors of date data.
#' @description The date format changes sometimes when I export the data from Access, sometimes exported as, for example, 1/28/13 0:00.
#' 
#' @export

Format_Date_Function <- function(x){
	if (nchar(as.character(x[1])) < 17) 
		{x = as.Date(x, "%m/%d/%y")} else 
		{x = as.Date(x, "%m/%d/%Y")}
	return(x)
}