#' Calculate Insect Variables
#' @param x the dataset
#' @param arrange.variable is variable that arranges the data. Defaults to "Date".
#' @param grouping.variable is the variable that groups the data. Defaults to "PlantID".
#' @description Calculate insects at time t ("Insect_t") and native insects at time t ("NatInsect_t").
#' @export

createNewInsectVariables <- function(
	x, 
	arrange.variable = "Date", 
	grouping.variable = "PlantID"
) {
	x %>%
		arrange_(.dots=arrange.variable) %>%
		group_by_(.dots=c(arrange.variable, grouping.variable)) %>%
		mutate(
			# new insect variables
			Insect_t 	= ifelse(
				sum(
					DA_t, 
					CA_t, 
					CH_t, 
					ME_t, 
					Unknown_Moth_t,
					Gerstaeckeria_t,
					na.rm=T
				)>0, 1, 0
			),
			NatInsect_t = ifelse(sum(DA_t, CH_t, ME_t, na.rm=T)>0, 1, 0)
		)
}