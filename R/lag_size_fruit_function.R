#' Calculate size and fruit production variables at the previous time step
#' @param x
#' @description calculate lag size and fruit.

lag_size_fruit_function <- function(x=x){
	x %>% 
		dplyr::arrange(Date) %>%
		dplyr::group_by(PlantID) %>%
		dplyr::mutate(
			# size
			Size_t_1 			= c(NA, utils::head(Size_t, -1)),
			Height_t_1 			= c(NA, utils::head(Height_t, -1)),
			Cone_t_1 			= c(NA, utils::head(Cone_t, -1)),
			Cylinder_Tall_t_1 	= c(NA, utils::head(Cylinder_Tall_t, -1)),
			# fruit
			Fruit_t_1 			= c(NA, utils::head(Fruit_t, -1)),
			FruitPres_t_1 		= c(NA, utils::head(FruitPres_t, -1))
		)
}
