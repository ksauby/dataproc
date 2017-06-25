#' Calculate lags on a grouped dataframe
#' @param x the dataset
#' @param vars the vector of variables
#' @param arrange.variable is variable that arranges the data. Defaults to "Date".
#' @param grouping.variable is the variable that groups the data. Defaults to "PlantID".
#' @description the function first arranges by Date
#' @export

calculateLagGroupedDF <- function(
	x, 
	vars, 
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
			mycols <- c(arrange.variable, vars[i])
			z <- x %>% dplyr::select_(
				.dots = c(
					grouping.variable,
					names(.)[match(mycols, names(x))]
				)
			)
			# set new variable name
			var.names <- setNames(vars[i], paste0(vars[i], "_1"))
			# calculate new lag variable
			z %<>% mutate_at(vars(var.names), funs(lag))
			y <- merge(y, z, by=c(grouping.variable, arrange.variable, vars[i]))
		}
	}
	y %<>% ungroup()
	return(y)
}
	
#' Calculate lag dates: Previous_Survey_Date, DaysSincePrevSurvey, DaysSinceStart
#' 
#' @param x Dataset
#' @description calculate lag dates.
#' @importFrom utils head
#' @import chron
#' @export

calculateDateLags <- function(x){
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
#' @param arrange.variable is variable that arranges the data
#' @param grouping.variable is the variable that groups the data
#' @description calculate lag insects.
#' 
#' @export

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
		"NatInsectyr_t",
		"Old_Moth_Evidence_t",
		"Unknown_Moth_t",
		"Gerstaeckeria_t",
		"Moth_Evidence_t",
		"Insect_Evidence_t"
	)
	x %>% calculateLagGroupedDF(
		vars=vars, 
		arrange.variable=arrange.variable, 
		grouping.variable=grouping.variable
	)
}

#' Calculate insect presence at time lag 2
#' 
#' @param x Dataset
#' @param arrange.variable is variable that arranges the data
#' @param grouping.variable is the variable that groups the data
#' @description calculate lag insects.
#' 
#' @export

calculateInsectLags2 <- function(
	x=x,
	arrange.variable, 
	grouping.variable
	){
	vars = c(
		"CA_t_1", 
		"ME_t_1", 
		"Old_Moth_Evidence_t_1",
		"Insect_t_1"
	)
	x %>% calculateLagGroupedDF(
		vars=vars, 
		arrange.variable=arrange.variable, 
		grouping.variable=grouping.variable
	)
}

#' Calculate insect presence at time lag 3
#' 
#' @param x Dataset
#' @param arrange.variable is variable that arranges the data
#' @param grouping.variable is the variable that groups the data
#' @description calculate lag insects.
#' 
#' @export

calculateInsectLags3 <- function(
	x=x,
	arrange.variable, 
	grouping.variable
	){
	vars = c(
		"CA_t_1_1", 
		"ME_t_1_1", 
		"Old_Moth_Evidence_t_1_1",
		"Insect_t_1_1"
	)
	x %>% calculateLagGroupedDF(
		vars=vars, 
		arrange.variable=arrange.variable, 
		grouping.variable=grouping.variable
	)
}

#' Calculate size variables at the previous time step
#' 
#' @param x Dataset
#' @param arrange.variable is variable that arranges the data
#' @param grouping.variable is the variable that groups the data
#' @description calculate lag size and fruit.
#' 
#' @export

calculateSizeLags <- function(
	x=x,
	arrange.variable, 
	grouping.variable,
	vars = c(
		"Size_t", 
		"Height_t", 
		"Cone_t", 
		"Cylinder_Tall_t", 
		"Cylinder_t", 
		"Elliptic_Cylinder_t",
		"Size_max_t",
		"Size_min_t",
		"Cone_max_t",
		"Cone_min_t",
		"Cylinder_Tall_max_t",
		"Cylinder_Tall_min_t"
	)
){
	x %>% calculateLagGroupedDF(
		vars, 
		arrange.variable, 
		grouping.variable
	)
}

#' Calculate fruit production variables at the previous time step
#' 
#' @param x Dataset
#' @param arrange.variable is variable that arranges the data
#' @param grouping.variable is the variable that groups the data

#' @description calculate lag size and fruit.
#' 
#' @export

calculateFruitLags <- function(
	x=x,
	arrange.variable, 
	grouping.variable
){
	vars <- c(
		"Fruit_t",
		"FruitPres_t"
	)
	x %>% calculateLagGroupedDF(
		vars=vars, 
		arrange.variable=arrange.variable, 
		grouping.variable=grouping.variable
	)
}