#' Create Dataset with All Surveys
#' @description Create dataset from all surveys and calculate:
#' \itemize{
#'  \item lagged fruit values
#'  \item lagged dates
#'  \item lagged insect presence
#'  \item relative growth rate
#' }
#' @param timeseries
#' @export

createAllSurveysDataset <- function(timeseries) {
	timeseries_all_surveys <- timeseries
	timeseries_all_surveys %<>% 
								# lag variables
								lag_size_function %>%
								lag_dates_function %>%
								lag_insects_function %>%
								# RGR
								RGR_function
	# Save
	setwd("/Users/KSauby/Documents/Dropbox/GradSchool/Research/Projects/marsico-time-series/")
	cache("timeseries_all_surveys")
	return(timeseries_all_surveys)
}	

#' Create Create a Fecundity Year Variable
#' @description Each starts on the first day of spring of that calendar year, then ends on the last day of winter in the next calendar year (e.g., Spring 2009 - Winter 2010).
#' The years are defined as follows:
#' \itemize{
#'  \item Year 1 - Spring, Summer, Fall, Winter 2009, Winter 2010
#'  \item Year 2 - Spring, Summer, Fall, Winter 2010, Winter 2011
#'  \item Year 3 - Spring, Summer, Fall, Winter 2011, Winter 2012
#'  \item Year 4 - Spring, Summer, Fall, Winter 2012, Winter 2013
#'	\item Year 5 - Spring, Summer, Fall, Winter 2013, Winter 2014
#' }
#' @param timeseries
#' @export

createObsYear <- function(timeseries) {
	timeseries$ObsYear <- NA
	timeseries %>% 
		as.data.frame %>%
		group_by(Date) %>% 
		mutate(
			ObsYear = replace(
				ObsYear, 
				which(
					(Season == "Spring" | 
					Season 	== "Summer" | 
					Season 	== "Fall") &
					Year 	== 2009 
				), 
				2009
			),
			ObsYear = replace(
				ObsYear, 
				which(
					(Season == "Spring" | 
					Season 	== "Summer" | 
					Season 	== "Fall") &
					Year 	== 2010 
				), 
				2010
			),
			ObsYear = replace(
				ObsYear, 
				which(
					(Season 	== "Spring" | 
					Season 	== "Summer" | 
					Season 	== "Fall") &
					Year 	== 2011
				), 
				2011
			),
			ObsYear = replace(
				ObsYear, 
				which(
					(Season 	== "Spring" | 
					Season 	== "Summer" | 
					Season 	== "Fall") &
					Year 	== 2012
				), 
				2012
			),
			ObsYear = replace(
				ObsYear, 
				which(
					(Season 	== "Spring" | 
					Season 	== "Summer" | 
					Season 	== "Fall") &
					Year 	== 2013
				), 
				2013
			),
			ObsYear = replace(
				ObsYear, 
				which(
					Date 	>= "2009-12-21" &
					Date 	< "2010-3-20"
				), 
				2009
			),
			ObsYear = replace(
				ObsYear, 
				which(
					Date 	>= "2010-12-21" &
					Date 	< "2011-3-20"
				), 
				2010
			),
			ObsYear = replace(
				ObsYear, 
				which(
					Date 	>= "2011-12-22" &
					Date 	< "2012-3-20"
				), 
				2011
			),
			ObsYear = replace(
				ObsYear, 
				which(
					Date 	>= "2012-12-21" &
					Date 	< "2013-3-20"
				), 
				2012
			),
			ObsYear = replace(
				ObsYear, 
				which(
					Date 	>= "2013-12-21" &
					Date 	< "2014-3-20"
				), 
				2013
			)
		) %>%
		ungroup
}

#' Create Dataset with Yearly Fruit Surveys
#' @description Create a dataset with yearly observations of fruit, size, and insect observations. Each starts on the first day of spring of that calendar year, then ends on the last day of winter in the next calendar year (e.g., Spring 2009 - Winter 2010). Variables that are calculated included the maximum number of fruit observed, insect presence/absence during the year, and the maximum and minimum plant size that year. All years, except for 2009, have two observations per year.
#' The dataset is created according to the following steps:
#' \itemize{
#'  \item create a year variable (called "ObsYear")
#'  \item determine if insect was ever observed during the ObsYear
#'  \item calculate maximum and minimum plant size and volume
#'  \item determine if surveys were complete for the entire year (no missing information)
#' }
#' @param timeseries
#' @export

createFruitYearDataset <- function(timeseries) {
	timeseries_fruityear <- timeseries
	timeseries_fruityear %>% 
		as.data.frame %>%
		createObsYear %>%
		filter(!(is.na(ObsYear))) %>%
		arrange(ObsYear) %>%
		group_by(PlantID) %>%
		mutate(PrevObsYear = ObsYear - 1) %>%
		ungroup %>%
		group_by(PlantID, ObsYear) %>%
		summarise(
			PrevObsYear				= PrevObsYear[1],
			Location 				= Location[1],
			Species					= Species[1],
			Fruit_t 				= max(Fruit_t),
			FruitPres_t 			= max(FruitPres_t),
			ME_t 					= max(ME_t),
			CA_t 					= max(CA_t),
			CH_t 					= max(CH_t),
			DA_t 					= max(DA_t),
			Size_t_max 				= max(Size_t),
			Cone_t_max 				= max(Cone_t), 
			Cylinder_Tall_t_max 	= max(Cylinder_Tall_t),
			Size_t_min 				= min(Size_t),
			Cone_t_min 				= min(Cone_t), 
			Cylinder_Tall_t_min 	= min(Cylinder_Tall_t),
			complete_insect_surveys = min(complete_insect_surveys), 
			complete_surveys		= min(complete_surveys)
		) %>%
	# lag variables
	lag_size_fruit_function
}