#' Abbreviate weather variable names
#' @param y
change_weather_names_function1 <- function(y) {	
	# precip
	setnames(y, "Daily_Precip_mean", 				"Daily Precip, mean")
	setnames(y, "Daily_Precip_SD", 					"Daily Precip, SD")
	setnames(y, "Perc_Days_w_Rain", 				"Perc. Days w/Rain")
	setnames(y, "Mean_Consecutive_Days_w_Rain", 	"Consec. Days w/Rain, Mean")
	setnames(y, "Max_Consecutive_Days_w_Rain", 		"Consec. Days w/Rain, Max")
	setnames(y, "sd_Consecutive_Days_w_Rain", 		"Consec. Days w/Rain, SD")
	setnames(y, "Mean_Consecutive_Drought_Days", 	"Consec. Days w/o Rain, Mean")
	setnames(y, "Max_Consecutive_Drought_Days", 	"Consec. Days w/o Rain, Max")
	setnames(y, "sd_Consecutive_Drought_Days",	 	"Consec. Days w/o Rain, SD")
	# temp
	setnames(y, "mean_Max_Temp", 					"Max Temp, Mean")
	setnames(y, "sd_Max_Temp", 						"Max Temp, SD")
	setnames(y, "MeanDegreeDay", 					"Mean Degree Day")
	setnames(y, "Perc_Days_w_Freeze", 				"Perc. Freezing Days")
	setnames(y, "Mean_Consecutive_Freezing_Days", 	"Consec. Freezing Days, Mean")
	setnames(y, "Max_Consecutive_Freezing_Days", 	"Consec. Freezing Days, Max")
	setnames(y, "sd_Consecutive_Freezing_Days", 	"Consec. Freezing Days, SD")
	return(y)
}
