#' survival rescale function
#' 
#' @param x x
#' @description rescale survival data
#'
#' @export
#' @importFrom scales rescale

survival_rescale_function <- function(x=x){
	mutate(x, 
		# plant size
		Ln_Size_mean_st = rescale(log(.data$Size_mean)),
		Ln_Size_t_1_st = rescale(log(.data$Size_t_1)),
		Ln_Size_min_st = rescale(log(.data$Size_min))		
)}
