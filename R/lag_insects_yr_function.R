#' Calculate insect presence during the previous year
#' @param x
#' @description calculate lag insects during the previous year.

lag_insects_yr_function <- function(x=x){
	x %>% 
		arrange(Date) %>%
		group_by(PlantID) %>%
		mutate(
			# new insect variables
			Insectyr_t 		= ifelse(sum(DAyr_t, CAyr_t, CHyr_t, MEyr_t, na.rm=T)>0, 1, 0),
			NatInsectyr_t 	= ifelse(sum(DAyr_t, CHyr_t, MEyr_t, na.rm=T)>0, 1, 0),
			# lagged insects
			CAyr_t_1 		= c(NA, head(CAyr_t, -1)),
			MEyr_t_1 		= c(NA, head(MEyr_t, -1)),
			CHyr_t_1 		= c(NA, head(CHyr_t, -1)),
			DAyr_t_1 		= c(NA, head(DAyr_t, -1)),
			Insectyr_t_1 		= c(NA, head(Insectyr_t, -1)),
			NatInsectyr_t_1 	= c(NA, head(NatInsectyr_t, -1))
		) %>%
		as.data.frame
}
