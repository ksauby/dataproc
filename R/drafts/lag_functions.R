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


lag_insect_function

Date or ObsYear

calculateInsectLags <- function(
	x=x,
	arrange.variable, 
	grouping.variable
	){
	vars = c(
		"CA_t", 
		"ME_t", 
		"CH_t", 
		"DA_t", 
		"Insect_t", 
		"NatInsect_t",
		"CAyr_t",
		"MEyr_t",
		"CHyr_t",
		"DAyr_t",
		"Insectyr_t",
		"NatInsectyr_t"
	)
	x %>% 
		arrange(Date) %>%
		group_by(PlantID, Date) %>%
		mutate(
			# new insect variables
			Insect_t 	= ifelse(sum(DA_t, CA_t, CH_t, ME_t, na.rm=T)>0, 1, 0),
			NatInsect_t = ifelse(sum(DA_t, CH_t, ME_t, na.rm=T)>0, 1, 0)
		) %>%
		group_by(PlantID) %>%
		calculateLagGroupedDF(
			vars=vars, 
			arrange.variable=arrange.variable, 
			grouping.variable=grouping.variable
		)
}



#' Calculate size variables at the previous time step
#' 
#' @param x Dataset
#' @description calculate lag size and fruit.
#' 
#' @export

lag_size_function <- function(x=x){
	
	
	vars <- c(
		"Size_t", 
		"Height_t", 
		"Cone_t", 
		"Cylinder_Tall_t", 
		"Cylinder_t", 
		"Elliptic_Cylinder_t"
	)
	
	
	x <- Plant_Surveys

	#' @param x the dataset
	#' @param vars the vector of variables
	#' @param arrange.variable is variable that arranges the data. Defaults to "Date".
	#' @param grouping.variable is the variable that groups the data. Defaults to "PlantID".
	#' @description the function first arranges by Date
	
	calculateLagGroupedDF <- function(
		x, 
		vars = vars, 
		arrange.variable = "Date", 
		grouping.variable = "PlantID"
	) {
		x %<>% 
			arrange_(.dots = arrange.variable) %>%
			group_by_(.dots = grouping.variable)
			# make a copy to use in the iterations
		y <- x
		# calculate lag for each variable in vars
		for (i in 1:length(vars)) {
			if (vars[i] %in% names(x)) {
				# select columns
				mycols <- c("Date", vars[i])
				z <- x %>% dplyr::select(match(mycols, names(.)))
				# set new variable name
				var.names <- setNames(vars[i], paste0(vars[i], "_1"))
				# calculate new lag variable
				z %<>% mutate_each_(funs(lag), var.names)
				y <- merge(y, z, by=c("PlantID", "Date", vars[i]))
			}
		}
		return(y)
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	vars <- c(
		"Size_t", 
		"Height_t", 
		"Cone_t", 
		"Cylinder_Tall_t", 
		"Cylinder_t", 
		"Elliptic_Cylinder_t"
		"Size_max_t_1",
		"Size_min_t_1",
		"Cone_max_t_1",
		"Cylinder_Tall_max_t_1"
	)
	
	
		


#' Calculate size and fruit production variables at the previous time step
#' 
#' @param x Dataset
#' @description calculate lag size and fruit.
#' 
#' @export
lag_size_fruit_function

calculateFruitLags <- function(
	x=x,
	arrange.variable, 
	grouping.variable
){
	vars = c(
		"Fruit_t",
		"FruitPres_t"
	)
	x %>% 
		arrange_(.dots=arrange.variable) %>%
		group_by_(.dots=grouping.variable) %>%
		calculateLagGroupedDF(
			vars=vars, 
			arrange.variable=arrange.variable, 
			grouping.variable=grouping.variable
		)
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
