#' Rescale Function

#' @description Only rescale once the dataset is final (missing values have been removed)
#' @param x
#' @export

rescale_function <- function(x=x){
	mutate(x, 
		# plant síze
		Ln_Size_t_1_st = arm::rescale(log(Size_t_1)),
		Ln_Cone_t_1_st = arm::rescale(log(Cone_t_1)),
		Ln_Cylinder_Tall_t_1_st = arm::rescale(log(Cylinder_Tall_t_1))
)}