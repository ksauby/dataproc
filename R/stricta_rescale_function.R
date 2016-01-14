#' Rescale Function
#'
#' @param x Dataset
#' @description Only rescale once the dataset is final (missing values have been removed); the rescale function is difference for stricta - it rescales current size rather than previous size due to the small size of the dataset.
#'
#' @export

stricta_rescale_function <- function(x=x){
	mutate(x, 
		# plant síze
		Ln_Size_t_1_st = rescale(log(Size_t_1)),
		Ln_Cone_t_1_st = rescale(log(Cone_t_1)),
		Ln_Cylinder_Tall_t_1_st = rescale(log(Cylinder_Tall_t_1))
)}