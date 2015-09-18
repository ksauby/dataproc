survival_rescale_function <- function(x=x){
	mutate(x, 
		# plant size
		Ln_Size_mean_st = rescale(log(Size_mean)),
		Ln_Size_t_1_st = rescale(log(Size_t_1)),
		Ln_Size_min_st = rescale(log(Size_min))		
)}
