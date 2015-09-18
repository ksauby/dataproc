#' Calculate size and fruit production variables at the previous time step
#' @param x
lag_size_fruit_function <- function(x=x){
	x %>% 
		arrange(Date) %>%
		group_by(PlantID) %>%
		mutate(
			# size
			Size_t_1 			= c(NA, head(Size_t, -1)),
			Height_t_1 			= c(NA, head(Height_t, -1)),
			Cone_t_1 			= c(NA, head(Cone_t, -1)),
			Cylinder_Tall_t_1 	= c(NA, head(Cylinder_Tall_t, -1)),
			# fruit
			Fruit_t_1 			= c(NA, head(Fruit_t, -1)),
			FruitPres_t_1 		= c(NA, head(FruitPres_t, -1))
		)
}
