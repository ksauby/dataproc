#' Rescale Function
#' 
#' @description Only rescale once the dataset is final (missing values have been removed)
#' @param x Dataset
#'
#' @export

rescale_function <- function(x=x){
	mutate(x, 
		# plant size
		Ln_Size_t_1_st = arm::rescale(log(.data$Size_t_1)),
		Ln_Cone_t_1_st = arm::rescale(log(.data$Cone_t_1)),
		Ln_Cylinder_Tall_t_1_st = arm::rescale(log(.data$Cylinder_Tall_t_1))
)}

#' Fecundity Rescale Function
#' 
#' @description Only rescale once the fecundity dataset is final (missing values have been removed)
#' @param x data?
#'
#' @export

fecundity_rescale_function <- function(x=x){
	mutate(x, 
		# plant size  
		Ln_size_max_t_1_st = arm::rescale(log(.data$Size_max_t_1)),
		Ln_size_min_t_1_st = arm::rescale(log(.data$Size_min_t_1))
)}