#' Abbreviate weather variable names
#' 
#' @param y Dataset
#' @description Abbreviate weather variable names
#' 
#' @export

change_weather_names_function1 <- function(y) {	
	# precip
	setnames(y, "Daily_Precip_mean", "Daily Precip, mean")
	setnames(y, "Daily_Precip_SD", "Daily Precip, SD")
	setnames(y, "Perc_Days_w_Rain", "Perc. Days w/Rain")
	setnames(y, "Mean_Consecutive_Days_w_Rain", "Consec. Days w/Rain, Mean")
	setnames(y, "Max_Consecutive_Days_w_Rain", "Consec. Days w/Rain, Max")
	setnames(y, "sd_Consecutive_Days_w_Rain", "Consec. Days w/Rain, SD")
	setnames(y, "Mean_Consecutive_Drought_Days", "Consec. Days w/o Rain, Mean")
	setnames(y, "Max_Consecutive_Drought_Days", "Consec. Days w/o Rain, Max")
	setnames(y, "sd_Consecutive_Drought_Days", "Consec. Days w/o Rain, SD")
	# temp
	setnames(y, "mean_Max_Temp", "Max Temp, Mean")
	setnames(y, "sd_Max_Temp", "Max Temp, SD")
	setnames(y, "MeanDegreeDay", "Mean Degree Day")
	setnames(y, "Perc_Days_w_Freeze", "Perc. Freezing Days")
	setnames(y, "Mean_Consecutive_Freezing_Days", "Consec. Freezing Days, Mean")
	setnames(y, "Max_Consecutive_Freezing_Days", "Consec. Freezing Days, Max")
	setnames(y, "sd_Consecutive_Freezing_Days", "Consec. Freezing Days, SD")
	return(y)
}

#' Abbreviate weather variable names to two characters
#' 
#' @param y Dataset
#' @description Abbreviate weather variable names to two characters
#' 
#' @export

change_weather_names_function2 <- function(y) {	
	# precip
	if ("Daily_Precip_mean" %in% names(y)) {
		setnames(y, "Daily_Precip_mean", 				"A1")
	}
	if ("Daily_Precip_SD" %in% names(y)) {
		setnames(y, "Daily_Precip_SD", 					"A2")
	}
	if ("Perc_Days_w_Rain" %in% names(y)) {
		setnames(y, "Perc_Days_w_Rain", 				"B")
	}
	if ("Mean_Consecutive_Days_w_Rain" %in% names(y)) {
		setnames(y, "Mean_Consecutive_Days_w_Rain", 	"C1")
	}
	if ("Max_Consecutive_Days_w_Rain" %in% names(y)) {
		setnames(y, "Max_Consecutive_Days_w_Rain", 		"C2")
	}
	if ("sd_Consecutive_Days_w_Rain" %in% names(y)) {
		setnames(y, "sd_Consecutive_Days_w_Rain", 		"C3")
	}
	if ("Mean_Consecutive_Drought_Days" %in% names(y)) {
		setnames(y, "Mean_Consecutive_Drought_Days", 	"D1")
	}
	if ("Max_Consecutive_Drought_Days" %in% names(y)) {
		setnames(y, "Max_Consecutive_Drought_Days", 	"D2")
	}
	if ("sd_Consecutive_Drought_Days" %in% names(y)) {
		setnames(y, "sd_Consecutive_Drought_Days",	 	"D3")
	}
	# temp
	if ("mean_Max_Temp" %in% names(y)) {
		setnames(y, "mean_Max_Temp", 					"E1")
	}
	if ("sd_Max_Temp" %in% names(y)) {
		setnames(y, "sd_Max_Temp", 						"E2")
	}
	if ("MeanDegreeDay" %in% names(y)) {
		setnames(y, "MeanDegreeDay", 					"F")
	}
	if ("Perc_Days_w_Freeze" %in% names(y)) {
		setnames(y, "Perc_Days_w_Freeze", 				"G")
	}
	if ("Mean_Consecutive_Freezing_Days" %in% names(y)) {
		setnames(y, "Mean_Consecutive_Freezing_Days", 	"H1")
	}
	if ("Max_Consecutive_Freezing_Days" %in% names(y)) {
		setnames(y, "Max_Consecutive_Freezing_Days", 	"H2")
	}
	if ("sd_Consecutive_Freezing_Days" %in% names(y)) {
		setnames(y, "sd_Consecutive_Freezing_Days", 	"H3")
	}
	return(y)
}

#' Abbreviate weather variable names
#' 
#' @param y Dataset
#' @description Abbreviate weather variable names
#' 
#' @export

change_weather_names_function3 <- function(y) {	
	# precip
	setnames(y, "Daily_Precip_mean", "D.Precip.mean")
	setnames(y, "Daily_Precip_SD", "D.Precip.SD")
	setnames(y, "Perc_Days_w_Rain", "% D w/Rain")
	setnames(y, "Mean_Consecutive_Days_w_Rain", "D w/Rain, Mean")
	setnames(y, "Max_Consecutive_Days_w_Rain", "D w/Rain, Max")
	setnames(y, "sd_Consecutive_Days_w_Rain", "D w/Rain, SD")
	setnames(y, "Mean_Consecutive_Drought_Days", "D w/o Rain, Mean")
	setnames(y, "Max_Consecutive_Drought_Days", "D w/o Rain, Max")
	setnames(y, "sd_Consecutive_Drought_Days", "D w/o Rain, SD")
	# temp
	setnames(y, "mean_Max_Temp", "Max Temp, Mean")
	setnames(y, "sd_Max_Temp", "Max Temp, SD")
	setnames(y, "MeanDegreeDay", "Mean Degree D")
	setnames(y, "Perc_Days_w_Freeze", "% Freez. D")
	setnames(y, "Mean_Consecutive_Freezing_Days", "Freez. D, Mean")
	setnames(y, "Max_Consecutive_Freezing_Days", "Freez. D, Max")
	setnames(y, "sd_Consecutive_Freezing_Days", "Freez. D, SD")
	return(y)
}
