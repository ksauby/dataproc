#' Rescale Function

#' @description Only rescale once the dataset is final (missing values have been removed); the rescale function is difference for stricta - it rescales current size rather than previous size due to the small size of the dataset.

stricta_rescale_function <- function(x=x){
	mutate(x, 
		# plant síze
		Ln_Size_t_st = rescale(log(Size_t)),
		Ln_Cone_t_st = rescale(log(Cone_t)),
		Ln_Cylinder_Tall_t_st = rescale(log(Cylinder_Tall_t))
)}