#' Abbreviate weather variable names
#' @param y
change_weather_names_function3 <- function(y) {	
	# precip
	setnames(y, "Daily_Precip_mean", 				"D.Precip.mean")
	setnames(y, "Daily_Precip_SD", 					"D.Precip.SD")
	setnames(y, "Perc_Days_w_Rain", 				"% D w/Rain")
	setnames(y, "Mean_Consecutive_Days_w_Rain", 	"D w/Rain, Mean")
	setnames(y, "Max_Consecutive_Days_w_Rain", 		"D w/Rain, Max")
	setnames(y, "sd_Consecutive_Days_w_Rain", 		"D w/Rain, SD")
	setnames(y, "Mean_Consecutive_Drought_Days", 	"D w/o Rain, Mean")
	setnames(y, "Max_Consecutive_Drought_Days", 	"D w/o Rain, Max")
	setnames(y, "sd_Consecutive_Drought_Days",	 	"D w/o Rain, SD")
	# temp
	setnames(y, "mean_Max_Temp", 					"Max Temp, Mean")
	setnames(y, "sd_Max_Temp", 						"Max Temp, SD")
	setnames(y, "MeanDegreeDay", 					"Mean Degree D")
	setnames(y, "Perc_Days_w_Freeze", 				"% Freez. D")
	setnames(y, "Mean_Consecutive_Freezing_Days", 	"Freez. D, Mean")
	setnames(y, "Max_Consecutive_Freezing_Days", 	"Freez. D, Max")
	setnames(y, "sd_Consecutive_Freezing_Days", 	"Freez. D, SD")
	return(y)
}
