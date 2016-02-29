#' Calculate lag dates: Previous_Survey_Date, DaysSincePrevSurvey, DaysSinceStart
#' 
#' @param x Dataset
#' @description calculate lag dates.
#' 
#' @importFrom utils head
#' @import chron
#' @export

lag_dates_function <- function(x){
	# print warning about duplicates in the dataset
	duplicates <- x %>% 
		group_by(PlantID, Date) %>%
		summarise(n.obs = length(Species)) %>%
		filter(n.obs > 1)
	if (dim(duplicates)[1] > 0) {
		stop("Duplicates observations for a PlantID, Date combination are present in the dataset.")
	} else {
		x %<>% 
			arrange(Date) %>%
			group_by(PlantID) %>%
			mutate(
				# previous dates
				# Previous_Survey_Date = as.Date(c(NA, utils::head(Date, -1))),
				Previous_Survey_Date = as.Date(lag(Date), origin="1970-01-01"),
				DaysSincePrevSurvey 	= Date - Previous_Survey_Date,
				DaysSinceStart 			= Date - Date[1]
			)
		x$DaysSincePrevSurvey 	%<>% as.numeric
		x$DaysSinceStart 		%<>% as.numeric
		return(x)
	}
}

#' Calculate insect presence at previous time step
#' 
#' @param x Dataset
#' @description calculate lag insects.
#' 
#' @export

lag_insects_function <- function(x=x){
	x %>% 
		arrange(Date) %>%
		group_by(PlantID, Date) %>%
		mutate(
			# new insect variables
			Insect_t 		= ifelse(sum(DA_t, CA_t, CH_t, ME_t, na.rm=T)>0, 1, 0),
			NatInsect_t 	= ifelse(sum(DA_t, CH_t, ME_t, na.rm=T)>0, 1, 0)
		) %>%
		group_by(PlantID) %>%
		mutate(
			# lagged insects
			CA_t_1 			= lag(CA_t),
			ME_t_1 			= lag(ME_t),
			# CH_t_1 			= lag(CH_t),
			# DA_t_1 			= lag(DA_t),
			Insect_t_1 		= lag(Insect_t),
			NatInsect_t_1 	= lag(NatInsect_t),
			Old_Moth_t_1 	= lag(Old_Moth_Evidence_t)
		) %>%
		as.data.frame
}


#' Calculate insect presence during the previous year
#' 
#' @param x Dataset
#' @description calculate lag insects during the previous year.
#' 
#' @export

lag_insects_yr_function <- function(x=x){
	x %>% 
		arrange(Date) %>%
		group_by(PlantID) %>%
		mutate(
			# new insect variables
			Insectyr_t 		= ifelse(sum(DAyr_t, CAyr_t, CHyr_t, MEyr_t, na.rm=T)>0, 1, 0),
			NatInsectyr_t 	= ifelse(sum(DAyr_t, CHyr_t, MEyr_t, na.rm=T)>0, 1, 0),
			# lagged insects
			CAyr_t_1 		= lag(CAyr_t),
			MEyr_t_1 		= lag(MEyr_t),
			CHyr_t_1 		= lag(CHyr_t),
			DAyr_t_1 		= lag(DAyr_t),
			Insectyr_t_1 		= lag(Insectyr_t),
			NatInsectyr_t_1 	= lag(NatInsectyr_t)
		) %>%
		as.data.frame
}


#' Calculate size variables at the previous time step
#' 
#' @param x Dataset
#' @description calculate lag size and fruit.
#' 
#' @export
lag_size_function <- function(x=x){
	x %>% 
		arrange(Date) %>%
		group_by(PlantID) %>%
		mutate(
			# size
			Size_t_1 				= lag(Size_t),
			Height_t_1 				= lag(Height_t),
			# fruit
			Fruit_t_1 			= lag(Fruit_t),
			FruitPres_t_1 		= lag(FruitPres_t)
		)
}


#' Calculate size and fruit production variables at the previous time step
#' 
#' @param x Dataset
#' @description calculate lag size and fruit.
#' 
#' @export
#' @importFrom dplyr lag

lag_size_fruit_function <- function(x=x){
	x %>% 
		arrange(ObsYear) %>%
		group_by(PlantID) %>%
		mutate(
			# size
			Size_max_t_1 			= lag(Size_t_max),
			Size_min_t_1 			= lag(Size_t_min),
			Cone_max_t_1 			= lag(Cone_t_max),
			Cylinder_Tall_max_t_1 	= lag(Cylinder_Tall_t_max),
			# fruit
			Fruit_t_1 			= lag(Fruit_t),
			FruitPres_t_1 		= lag(FruitPres_t)
		)
}

#' Calculate insect presence at previous time step for fecundity data
#' 
#' @param x Dataset
#' @description calculate lag insects.
#' 
#' @export

lag_insects_fecundity_function <- function(x=x){
	x %>% 
		arrange(ObsYear) %>%
		group_by(PlantID) %>%
		mutate(
			# new insect variables
			Insect_t 		= ifelse(sum(DA_t, CA_t, CH_t, ME_t, na.rm=T)>0, 1, 0),
			NatInsect_t 	= ifelse(sum(DA_t, CH_t, ME_t, na.rm=T)>0, 1, 0),
			# lagged insects
			CA_t_1 		= lag(CA_t),
			ME_t_1 		= lag(ME_t),
			# CH_t_1 		= lag(CH_t),
			# DA_t_1 		= lag(DA_t),
			Insect_t_1 		= lag(Insect_t),
			NatInsect_t_1 	= lag(NatInsect_t)
		) %>%
		as.data.frame
}

#' Calculate size and fruit production variables at the previous time step
#' 
#' @param x Dataset
#' @description calculate lag size and fruit.
#' 
#' @export

lag_size_fruit_function_GTMNERR <- function(x=x){
	x %>% 
		arrange(Date) %>%
		group_by(PlantID) %>%
		mutate(
			# size
			Size_t_1 				= lag(Size_t),
			Height_t_1 				= lag(Height_t),
			#Cone_t_1 				= lag(Cone_t),
			#Cylinder_t_1 			= lag(Cylinder_t),
			#Elliptic_Cylinder_t_1 	= lag(Elliptic_Cylinder_t),
			# fruit
			Fruit_t_1 			= lag(Fruit_t),
			FruitPres_t_1 		= lag(FruitPres_t)
		)
}
