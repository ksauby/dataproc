#' Calculate lag dates: Previous_Survey_Date, DaysSincePrevSurvey, DaysSinceStart
#' @param x
#' @description calculate lag dates.
lag_dates_function <- function(x=x){
	# print warning about duplicates in the dataset
	duplicates <- x %>% 
		group_by(PlantID, Date) %>%
		summarise(n.obs = length(Species)) %>%
		filter(n.obs > 1)
		if (dim(duplicates)[1] > 0) {
			stop("Duplicates observations for a PlantID, Date combination are present in the dataset.")
		} else {
			x %<>% 
				dplyr::arrange(Date) %>%
				dplyr::group_by(PlantID) %>%
				dplyr::mutate(
					# previous dates
					Previous_Survey_Date 	= base::as.Date(c(NA, utils::head(Date, -1))),
					DaysSincePrevSurvey 	= Date - Previous_Survey_Date,
					DaysSinceStart 			= Date - Date[1]
				)
			x$DaysSincePrevSurvey 	%<>% as.numeric
			x$DaysSinceStart 		%<>% as.numeric
	
	
	
			return(x)
			
		}
}
