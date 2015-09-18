#' Abbreviate weather variable names to two characters
#' @param y

change_weather_names_function2 <- function(y) {	
	# precip
	setnames(y, "Daily_Precip_mean", 				"A1")
	setnames(y, "Daily_Precip_SD", 					"A2")
	setnames(y, "Perc_Days_w_Rain", 				"B")
	setnames(y, "Mean_Consecutive_Days_w_Rain", 	"C1")
	setnames(y, "Max_Consecutive_Days_w_Rain", 		"C2")
	setnames(y, "sd_Consecutive_Days_w_Rain", 		"C3")
	setnames(y, "Mean_Consecutive_Drought_Days", 	"D1")
	setnames(y, "Max_Consecutive_Drought_Days", 	"D2")
	setnames(y, "sd_Consecutive_Drought_Days",	 	"D3")
	# temp
	setnames(y, "mean_Max_Temp", 					"E1")
	setnames(y, "sd_Max_Temp", 						"E2")
	setnames(y, "MeanDegreeDay", 					"F")
	setnames(y, "Perc_Days_w_Freeze", 				"G")
	setnames(y, "Mean_Consecutive_Freezing_Days", 	"H1")
	setnames(y, "Max_Consecutive_Freezing_Days", 	"H2")
	setnames(y, "sd_Consecutive_Freezing_Days", 	"H3")
	return(y)
}
