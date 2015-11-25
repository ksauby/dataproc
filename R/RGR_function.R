#' Calculate Relative Growth Rate
#' @param x
#' @description formula for relative growth rate is from Paine, C. E. T., Marthews, T. R., Vogt, D. R., Purves, D., Rees, M., Hector, A., & Turnbull, L. A. (2011). How to fit nonlinear plant growth models and calculate growth rates: an update for ecologists. Methods in Ecology and Evolution, no–no. doi:10.1111/j.2041-210X.2011.00155.x

#' absolute growth rate (AGR) is the derivative with respect to time of the function used to predict biomass, and RGR is simply AGR divided by the current biomass

#' RGR is (dM/dt)/M

#' CURRENT FORMULA FOR RGR
#' after meeting with Mary and Bob (1 Dec 2014) Bob suggested that I divide by plant size at time t-1

#' scale by DaysSincePrevTime
#' @export

RGR_function <- function(x=x){
	x %<>% mutate(
		RGR_Height 			= (Height_t - Height_t_1) /
								(DaysSincePrevSurvey*Height_t_1), 
	 	RGR_Size 			= (Size_t - Size_t_1) / 
								(DaysSincePrevSurvey*Size_t_1),
		RGR_Cone 			= (Cone_t - Cone_t_1) / 
								(DaysSincePrevSurvey*Cone_t_1),
		RGR_Cylinder_Tall 	= (Cylinder_Tall_t - Cylinder_Tall_t_1) / 
								(DaysSincePrevSurvey*Cylinder_Tall_t_1),
		RGR_Size365 				= RGR_Size*365,
		RGR_Cone365 				= RGR_Cone*365,
		RGR_CylinderTall365 		= RGR_Cylinder_Tall*365,
		# lagged RGR
		RGR_Height_t_1 			= c(NA, head(RGR_Height, -1)),
		RGR_Size_t_1 			= c(NA, head(RGR_Size, -1)),
		RGR_Cone_t_1 			= c(NA, head(RGR_Cone, -1)),
		RGR_Cylinder_Tall_t_1 	= c(NA, head(RGR_Cylinder_Tall, -1))		
	) %>%
	as.data.table
	# remove Inf values
	cnames <- c(
		"RGR_Height", 
		"RGR_Size", 
		"RGR_Cone",
		"RGR_Cylinder_Tall",
		"RGR_Size365",
		"RGR_Cone365",
		"RGR_CylinderTall365",
		"RGR_Height_t_1",
		"RGR_Size_t_1",
		"RGR_Cone_t_1",
		"RGR_Cylinder_Tall_t_1"
		)
	for (cname in cnames) {
		x[, cname := NA_Function(x[[cname]]), with=FALSE]
	}
	return(x)
}
