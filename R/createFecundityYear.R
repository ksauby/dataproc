#' Create Create a Fecundity Year Variable
#' 
#' @description Each starts on the first day of spring of that calendar year, then ends on the last day of winter in the next calendar year (e.g., Spring 2009 - Winter 2010).
#' The years are defined as follows:
#' \itemize{
#'  \item Year 2009 - Spring, Summer, Fall, Winter 2009 - 2010
#'  \item Year 2010 - Spring, Summer, Fall, Winter 2010 - 2011
#'  \item Year 2011 - Spring, Summer, Fall, Winter 2011 - 2012
#'  \item Year 2012 - Spring, Summer, Fall, Winter 2012 - 2013
#'	\item Year 2013 - Spring, Summer, Fall, Winter 2013 - 2014
#'	\item Year 2014 - Spring, Summer, Fall, Winter 2014 - 2015
#'	\item Year 2015 - Spring, Summer, Fall, Winter 2015 - 2016
#' }
#' @param timeseries Dataset
#' 
#' @export

createFecundityYear <- function(timeseries) {
	timeseries$FecundityYear <- NA
	timeseries %>% 
		as.data.frame %>%
		group_by(.data$Date) %>% 
		mutate(
			FecundityYear = replace(
				.data$FecundityYear, 
				which(
					(.data$Season == "Spring" | 
					.data$Season 	== "Summer" | 
					.data$Season 	== "Fall") &
					year(.data$Date) 	== 2009 
				), 
				2009
			),
			FecundityYear = replace(
				.data$FecundityYear, 
				which(
					(.data$Season == "Spring" | 
					.data$Season 	== "Summer" | 
					.data$Season 	== "Fall") &
					.data$year(Date)== 2010 
				), 
				2010
			),
			FecundityYear = replace(
				.data$FecundityYear, 
				which(
					(.data$Season == "Spring" | 
					.data$Season 	== "Summer" | 
					.data$Season 	== "Fall") &
					year(.data$Date)== 2011
				), 
				2011
			),
			FecundityYear = replace(
				.data$FecundityYear, 
				which(
					(.data$Season == "Spring" | 
					.data$Season 	== "Summer" | 
					.data$Season 	== "Fall") &
					year(.data$Date)== 2012
				), 
				2012
			),
			FecundityYear = replace(
				.data$FecundityYear, 
				which(
					(.data$Season == "Spring" | 
					.data$Season 	== "Summer" | 
					.data$Season 	== "Fall") &
					year(.data$Date)== 2013
				), 
				2013
			),
			FecundityYear = replace(
				.data$FecundityYear, 
				which(
					(.data$Season == "Spring" | 
					.data$Season 	== "Summer" | 
					.data$Season 	== "Fall") &
					year(.data$Date)== 2014
				), 
				2014
			),
			FecundityYear = replace(
				.data$FecundityYear, 
				which(
					(.data$Season == "Spring" | 
					.data$Season 	== "Summer" | 
					.data$Season 	== "Fall") &
					year(.data$Date)== 2015
				), 
				2015
			),
			FecundityYear = replace(
				.data$FecundityYear, 
				which(
					.data$Date 	>= "2009-12-21" &
					.data$Date 	< "2010-3-20"
				), 
				2009
			),
			FecundityYear = replace(
				.data$FecundityYear, 
				which(
					.data$Date 	>= "2010-12-21" &
					.data$Date 	< "2011-3-20"
				), 
				2010
			),
			FecundityYear = replace(
				.data$FecundityYear, 
				which(
					.data$Date 	>= "2011-12-22" &
					.data$Date 	< "2012-3-20"
				), 
				2011
			),
			FecundityYear = replace(
				.data$FecundityYear, 
				which(
					.data$Date 	>= "2012-12-21" &
					.data$Date 	< "2013-3-20"
				), 
				2012
			),
			FecundityYear = replace(
				.data$FecundityYear, 
				which(
					.data$Date 	>= "2013-12-21" &
					.data$Date 	< "2014-3-20"
				), 
				2013
			),
			FecundityYear = replace(
				.data$FecundityYear, 
				which(
					.data$Date 	>= "2014-12-21" &
					.data$Date 	< "2015-3-20"
				), 
				2014
			),
			FecundityYear = replace(
				.data$FecundityYear, 
				which(
					.data$Date 	>= "2015-12-21" &
					.data$Date 	< "2016-3-20"
				), 
				2015
			)
		) %>%
		ungroup
}
