#' Calculate lag dates: Previous_Survey_Date, DaysSincePrevSurvey, DaysSinceStart
#' @param x
#' @description calculate lag dates.
lag_dates_function <- function(x=x){
	x %<>% 
		arrange(Date) %>%
		group_by(PlantID) %>%
		mutate(
			# previous dates
			Previous_Survey_Date 	= as.Date(c(NA, head(Date, -1))),
			DaysSincePrevSurvey 	= Date - Previous_Survey_Date,
			DaysSinceStart 			= Date - Date[1]
		)
	x$DaysSincePrevSurvey 	%<>% as.numeric
	x$DaysSinceStart 		%<>% as.numeric
	return(x)
}
