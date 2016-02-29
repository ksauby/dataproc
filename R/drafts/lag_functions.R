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
				Previous_Survey_Date = as.Date(c(NA, Date[-length(Date)]), origin="1970-01-01"),
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
			CA_t_1 		= c(NA, head(CA_t, -1)),
			ME_t_1 		= c(NA, head(ME_t, -1)),
			CH_t_1 		= c(NA, head(CH_t, -1)),
			DA_t_1 		= c(NA, head(DA_t, -1)),
			Insect_t_1 		= c(NA, head(Insect_t, -1)),
			NatInsect_t_1 	= c(NA, head(NatInsect_t, -1))
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
			CAyr_t_1 		= c(NA, head(CAyr_t, -1)),
			MEyr_t_1 		= c(NA, head(MEyr_t, -1)),
			CHyr_t_1 		= c(NA, head(CHyr_t, -1)),
			DAyr_t_1 		= c(NA, head(DAyr_t, -1)),
			Insectyr_t_1 		= c(NA, head(Insectyr_t, -1)),
			NatInsectyr_t_1 	= c(NA, head(NatInsectyr_t, -1))
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
	x <- Plant_Surveys
	x %<>% 
		arrange(Date) %>%
		group_by(PlantID)
		sizes <- c(
			"Size_t", 
			"Height_t", 
			"Cone_t", 
			"Cylinder_Tall_t_1", 
			"Elliptic_Cylinder_t_1"
		)
		for (i in 1:length(sizes)) {
			if (sizes[i] %in% names(x)) {
			  #  varname <- 
				varval <- lazyeval::interp(~dplyr::lag(eval(parse(text=sizes[i]))), i=i)
			    x %<>% 
					group_by(PlantID) %>%
					mutate(
						.dots = setNames(
							list(varval),
							sizes[i]
						)
					)	
				}			
			names(x)[dim(x)[2]] <- paste(sizes[i], "_1", sep="")
		#		temp <- eval(parse(text=paste("x$", sizes[i], sep="")))
			}
		}	
		
		

"Fruit_t_1", 
"FruitPres_t_1", 


#' Calculate size and fruit production variables at the previous time step
#' 
#' @param x Dataset
#' @description calculate lag size and fruit.
#' 
#' @export

lag_size_fruit_function <- function(x=x){
	x %>% 
		arrange(ObsYear) %>%
		group_by(PlantID) %>%
		mutate(
			# size
			Size_max_t_1 			= c(NA, Size_t_max[-length(Size_t_max)]),
			Size_min_t_1 			= c(NA, Size_t_min[-length(Size_t_min)]),
			Cone_max_t_1 			= c(NA, Cone_t_max[-length(Cone_t_max)]),
			Cylinder_Tall_max_t_1 	= c(NA, Cylinder_Tall_t_max[-length(Cylinder_Tall_t_max)]),
			# fruit
			Fruit_t_1 			= c(NA, Fruit_t[-length(Fruit_t)]),
			FruitPres_t_1 		= c(NA, FruitPres_t[-length(FruitPres_t)])
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
			CA_t_1 		= c(NA, head(CA_t, -1)),
			ME_t_1 		= c(NA, head(ME_t, -1)),
			CH_t_1 		= c(NA, head(CH_t, -1)),
			DA_t_1 		= c(NA, head(DA_t, -1)),
			Insect_t_1 		= c(NA, head(Insect_t, -1)),
			NatInsect_t_1 	= c(NA, head(NatInsect_t, -1))
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
			Size_t_1 				= c(NA, head(Size_t, -1)),
			Height_t_1 				= c(NA, head(Height_t, -1)),
			Cone_t_1 				= c(NA, head(Cone_t, -1)),
			Cylinder_t_1 			= c(NA, head(Cylinder_t, -1)),
			Elliptic_Cylinder_t_1 	= c(NA, head(Elliptic_Cylinder_t, -1)),
			# fruit
			Fruit_t_1 			= c(NA, head(Fruit_t, -1)),
			FruitPres_t_1 		= c(NA, head(FruitPres_t, -1))
		)
}
