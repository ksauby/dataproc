#' Calculate Relative Growth Rate
#'
#' @param x Dataset to which RGR variables should be added.
#' @description formula for relative growth rate is from Paine, C. E. T., Marthews, T. R., Vogt, D. R., Purves, D., Rees, M., Hector, A., & Turnbull, L. A. (2011). How to fit nonlinear plant growth models and calculate growth rates: an update for ecologists. Methods in Ecology and Evolution, no–no. doi:10.1111/j.2041-210X.2011.00155.x

#' absolute growth rate (AGR) is the derivative with respect to time of the function used to predict biomass, and RGR is simply AGR divided by the current biomass

#' RGR is (dM/dt)/M

#' CURRENT FORMULA FOR RGR
#' after meeting with Mary and Bob (1 Dec 2014) Bob suggested that I divide by plant size at time t-1

#' scale by DaysSincePrevTime
#'
#' @export
#' @importFrom dplyr mutate
#' @import magrittr
#' @import data.table

calculateRGR <- function(x=x){
	vars <- c(
		"Height_t",
		"Size_t",
		"Cone_t",
		"Cylinder_Tall_t"
	)
	if ("Height_t" %in% names(x)) {
		x %<>% mutate(
			RGR_Height		= (Height_t - Height_t_1) /
								(DaysSincePrevSurvey*Height_t_1),
			RGR_Height365 	= RGR_Height*365		
		)
	}
	if ("Size_t" %in% names(x)) {
		x %<>% mutate(
			RGR_Size		= (Size_t - Size_t_1) /
								(DaysSincePrevSurvey*Size_t_1),
			RGR_Size365 	= RGR_Size*365				
		)
	}
	if ("Cone_t" %in% names(x)) {
		x %<>% mutate(
			RGR_Cone		= (Cone_t - Cone_t_1) / 
								(DaysSincePrevSurvey*Cone_t_1),
			RGR_Cone365 	= RGR_Cone*365				
		)
	}
	if ("Cylinder_Tall_t" %in% names(x)) {
		x %<>% mutate(
			RGR_Cylinder_Tall = (Cylinder_Tall_t - Cylinder_Tall_t_1) / 
									(DaysSincePrevSurvey*Cylinder_Tall_t_1),
			RGR_CylinderTall365 = RGR_Cylinder_Tall*365
		)
	}
	
	# remove Inf values
	# x[, names(x)] %<>%
	#	apply(., 2, NA_Function
	#)
	return(x)
}
