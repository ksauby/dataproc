#' Calculate insect presence at previous time step
#' @param x
#' @description calculate lag insects.

lag_insects_function <- function(x=x){
	x %>% 
		dplyr::arrange(Date) %>%
		dplyr::group_by(PlantID) %>%
		dplyr::mutate(
			# new insect variables
			Insect_t 		= ifelse(sum(DA_t, CA_t, CH_t, ME_t, na.rm=T)>0, 1, 0),
			NatInsect_t 	= ifelse(sum(DA_t, CH_t, ME_t, na.rm=T)>0, 1, 0),
			# lagged insects
			CA_t_1 		= c(NA, head(CA_t, -1)),
			ME_t_1 		= c(NA, head(ME_t, -1)),
			CH_t_1 		= c(NA, head(CH_t, -1)),
			DA_t_1 		= c(NA, head(DA_t, -1)),
			Insect_t_1 		= c(NA, head(Insect_t, -1)),
			NatInsect_t_1 	= c(NA, head(NatInsect_t, -1))
		) %>%
		as.data.frame
}
