#' Abbreviate weather variable names
#' @param y
change_weather_names_function3 <- function(y) {	
	# precip
	data.table::setnames(y, "Daily_Precip_mean", "D.Precip.mean")
	data.table::setnames(y, "Daily_Precip_SD", "D.Precip.SD")
	data.table::setnames(y, "Perc_Days_w_Rain", "% D w/Rain")
	data.table::setnames(y, "Mean_Consecutive_Days_w_Rain", 	
		"D w/Rain, Mean")
	data.table::setnames(y, "Max_Consecutive_Days_w_Rain", 		
		"D w/Rain, Max")
	data.table::setnames(y, "sd_Consecutive_Days_w_Rain", 		
		"D w/Rain, SD")
	data.table::setnames(y, "Mean_Consecutive_Drought_Days", 	
		"D w/o Rain, Mean")
	data.table::setnames(y, "Max_Consecutive_Drought_Days", 	
		"D w/o Rain, Max")
	data.table::setnames(y, "sd_Consecutive_Drought_Days",	 	
		"D w/o Rain, SD")
	# temp
	data.table::setnames(y, "mean_Max_Temp", "Max Temp, Mean")
	data.table::setnames(y, "sd_Max_Temp", "Max Temp, SD")
	data.table::setnames(y, "MeanDegreeDay", "Mean Degree D")
	data.table::setnames(y, "Perc_Days_w_Freeze", "% Freez. D")
	data.table::setnames(y, "Mean_Consecutive_Freezing_Days", 	
		"Freez. D, Mean")
	data.table::setnames(y, "Max_Consecutive_Freezing_Days", "Freez. D, Max")
	data.table::setnames(y, "sd_Consecutive_Freezing_Days", "Freez. D, SD")
	return(y)
}
