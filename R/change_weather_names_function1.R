#' Abbreviate weather variable names
#' @param y
change_weather_names_function1 <- function(y) {	
	# precip
	data.table::setnames(y, "Daily_Precip_mean", 				
		"Daily Precip, mean")
	data.table::setnames(y, "Daily_Precip_SD", 					
		"Daily Precip, SD")
	data.table::setnames(y, "Perc_Days_w_Rain", 				
		"Perc. Days w/Rain")
	data.table::setnames(y, "Mean_Consecutive_Days_w_Rain", 	
		"Consec. Days w/Rain, Mean")
	data.table::setnames(y, "Max_Consecutive_Days_w_Rain", 		
		"Consec. Days w/Rain, Max")
	data.table::setnames(y, "sd_Consecutive_Days_w_Rain", 		
		"Consec. Days w/Rain, SD")
	data.table::setnames(y, "Mean_Consecutive_Drought_Days", 	
		"Consec. Days w/o Rain, Mean")
	data.table::setnames(y, "Max_Consecutive_Drought_Days", 	
		"Consec. Days w/o Rain, Max")
	data.table::setnames(y, "sd_Consecutive_Drought_Days",	 	
		"Consec. Days w/o Rain, SD")
	# temp
	data.table::setnames(y, "mean_Max_Temp", "Max Temp, Mean")
	data.table::setnames(y, "sd_Max_Temp", "Max Temp, SD")
	data.table::setnames(y, "MeanDegreeDay", "Mean Degree Day")
	data.table::setnames(y, "Perc_Days_w_Freeze", "Perc. Freezing Days")
	data.table::setnames(y, "Mean_Consecutive_Freezing_Days", 	
		"Consec. Freezing Days, Mean")
	data.table::setnames(y, "Max_Consecutive_Freezing_Days", 	
		"Consec. Freezing Days, Max")
	data.table::setnames(y, "sd_Consecutive_Freezing_Days", 	
		"Consec. Freezing Days, SD")
	return(y)
}
