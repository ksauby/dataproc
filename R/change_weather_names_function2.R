#' Abbreviate weather variable names to two characters
#' @param y

change_weather_names_function2 <- function(y) {	
	# precip
	data.table::setnames(y, "Daily_Precip_mean", 				"A1")
	data.table::setnames(y, "Daily_Precip_SD", 					"A2")
	data.table::setnames(y, "Perc_Days_w_Rain", 				"B")
	data.table::setnames(y, "Mean_Consecutive_Days_w_Rain", 	"C1")
	data.table::setnames(y, "Max_Consecutive_Days_w_Rain", 		"C2")
	data.table::setnames(y, "sd_Consecutive_Days_w_Rain", 		"C3")
	data.table::setnames(y, "Mean_Consecutive_Drought_Days", 	"D1")
	data.table::setnames(y, "Max_Consecutive_Drought_Days", 	"D2")
	data.table::setnames(y, "sd_Consecutive_Drought_Days",	 	"D3")
	# temp
	data.table::setnames(y, "mean_Max_Temp", 					"E1")
	data.table::setnames(y, "sd_Max_Temp", 						"E2")
	data.table::setnames(y, "MeanDegreeDay", 					"F")
	data.table::setnames(y, "Perc_Days_w_Freeze", 				"G")
	data.table::setnames(y, "Mean_Consecutive_Freezing_Days", 	"H1")
	data.table::setnames(y, "Max_Consecutive_Freezing_Days", 	"H2")
	data.table::setnames(y, "sd_Consecutive_Freezing_Days", 	"H3")
	return(y)
}
