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
								lag_size_fruit_function %>%
								lag_dates_function %>%
								lag_insects_function %>%
								# RGR
								RGR_function
	# Save
	setwd("/Users/KSauby/Documents/Dropbox/GradSchool/Research/Projects/marsico-time-series/")
	cache("timeseries_all_surveys")
	return(timeseries_all_surveys)
}	
	
#' Create Dataset with Fall/Winter Surveys
#' @description Create dataset from fall/winter surveys and calculate:
#' \itemize{
#'  \item lagged fruit values
#'  \item lagged dates
#'  \item lagged insect presence
#'  \item relative growth rate
#' }
#' @param timeseries
#' @export
	
createFallWinterSurveysDataset <- function(timeseries) {
	timeseries_winter <- timeseries
	timeseries_winter$Visit <- "NA"
	timeseries_winter %<>% 
		as.data.frame %>%
		group_by(Date) %>% 
		mutate(
			Visit = replace(Visit, which(Date >= "2009-01-21" & 
				Date < "2010-11-15"), "Year2010"),
			Visit = replace(Visit, which(Date >= "2010-11-15" & 
				Date < "2011-12-15"), "Year2011"),
			Visit = replace(Visit, which(Date >= "2011-12-15" & 
				Date < "2012-12-14"), "Year2012"),
			Visit = replace(Visit, which(Date >= "2012-12-14" & 
				Date < "2014-01-12"), "Year2014"))
	new = timeseries_winter %>%
		select(Visit, PlantID, ME_t, CA_t, CH_t, DA_t) %>%
		group_by(Visit, PlantID) %>%
		summarise(
			CAyr_t 			= ifelse(sum(CA_t, na.rm=T)>0, 1, 0),
			MEyr_t 			= ifelse(sum(ME_t, na.rm=T)>0, 1, 0),
			CHyr_t 			= ifelse(sum(CH_t, na.rm=T)>0, 1, 0),
			DAyr_t 			= ifelse(sum(DA_t, na.rm=T)>0, 1, 0),
			Insectyr_t 		= ifelse(
				sum(DA_t, CA_t, CH_t, ME_t, na.rm=T) > 0, 1, 0),
			NatInsectyr_t 	= ifelse(sum(DA_t, CH_t, ME_t, na.rm=T)>0, 1, 0)
		)
	# use only winter data
	timeseries_winter %<>%
		mutate(Month=month(Date)) %>%
		arrange(Date) %>%
		filter(Month==10 | Month==11 | Month==12 | Month==1) %>%
		merge(new, by=c("Visit", "PlantID"), all.x=T)
	# create lag and RGR variables
	timeseries_winter %<>%
						# lag variables
						lag_size_fruit_function %>%
						lag_dates_function %>%
						lag_insects_yr_function %>%
						# RGR
						as.data.frame %>%
						RGR_function
	# Save
	setwd("/Users/KSauby/Documents/Dropbox/GradSchool/Research/Projects/marsico-time-series/")
	cache("timeseries_winter")
	return(timeseries_winter)
}	
		
#' Create Dataset with Spring/Summer Surveys
#' @description Create dataset from spring/summer surveys and calculate:
#' \itemize{
#'  \item lagged fruit values
#'  \item lagged dates
#'  \item lagged insect presence
#'  \item relative growth rate
#' }
#' @param timeseries
#' @export
	
createSpringSummerSurveysDataset <- function(timeseries) {
	timeseries_spring <- timeseries
	timeseries_spring$Visit <- "NA"
	timeseries_spring %<>% 
		as.data.frame %>%
		group_by(Date) %>% 
		mutate(
			Visit = replace(Visit, which(Date >= "2009-04-01" & 
				Date < "2010-05-01"), "Year1"),
			Visit = replace(Visit, which(Date >= "2010-05-01" & 
				Date < "2011-05-01"), "Year2"),
			Visit = replace(Visit, which(Date >= "2011-05-01" & 
				Date < "2012-05-01"), "Year3"),
			Visit = replace(Visit, which(Date >= "2012-05-01" & 
				Date < "2013-05-01"), "Year4"),
			Visit = replace(Visit, which(Date > "2013-05-01" ), "Year5"))
	new = timeseries_spring %>%
		select(Visit, PlantID, ME_t, CA_t, CH_t, DA_t) %>%
		group_by(Visit, PlantID) %>%
		summarise(
			CAyr_t 			= ifelse(sum(CA_t, na.rm=T)>0, 1, 0),
			MEyr_t 			= ifelse(sum(ME_t, na.rm=T)>0, 1, 0),
			CHyr_t 			= ifelse(sum(CH_t, na.rm=T)>0, 1, 0),
			DAyr_t 			= ifelse(sum(DA_t, na.rm=T)>0, 1, 0),
			Insectyr_t 		= ifelse(
				sum(DA_t, CA_t, CH_t, ME_t, na.rm=T) > 0, 1, 0),
			NatInsectyr_t 	= ifelse(sum(DA_t, CH_t, ME_t, na.rm=T)>0, 1, 0)
		)
	# use only spring data
	timeseries_spring %<>%
		mutate(Month=month(Date)) %>%
		arrange(Date) %>%
		filter(Month==4 | Month==5) %>%
		merge(new, by=c("Visit", "PlantID"), all.x=T)
	# create lag and RGR variables
	timeseries_spring %<>%
						# lag variables
						lag_size_fruit_function %>%
						lag_dates_function %>%
						lag_insects_yr_function %>%
						# RGR
						as.data.frame %>%
						RGR_function
	# Save
	setwd("/Users/KSauby/Documents/Dropbox/GradSchool/Research/Projects/marsico-time-series/")
	cache("timeseries_spring")
	return(timeseries_spring)
}

#' Create Dataset with Yearly Fruit Surveys
#' @description Create a dataset with yearly observations of fruit, size, and insect observations. Each starts on the first day of spring of that calendar year, then ends on the last day of winter in the next calendar year (e.g., Spring 2009 - Winter 2010). Variables that are calculated included the maximum number of fruit observed, insect presence/absence during the year, and the maximum and minimum plant size that year. All years, except for 2009, have two observations per year.
#' The years are defined as follows:
#' \itemize{
#'  \item Year 1 - Spring, Summer, Fall, Winter 2009, Winter 2010
#'  \item Year 2 - Spring, Summer, Fall, Winter 2010, Winter 2011
#'  \item Year 3 - Spring, Summer, Fall, Winter 2011, Winter 2012
#'  \item Year 4 - Spring, Summer, Fall, Winter 2012, Winter 2013
#'	\item Year 5 - Spring, Summer, Fall, Winter 2013, Winter 2014
#' }
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
	timeseries_fruityear$ObsYear <- "NA"
	timeseries_fruityear %>% 
		as.data.frame %>%
		group_by(Date) %>% 
		mutate(
			ObsYear = replace(
				ObsYear, 
				which(
					(Season 	== "Spring" | 
					Season 	== "Summer" | 
					Season 	== "Fall") &
					Year 	== 2009 
				), 
				"Year2009"
			),
			ObsYear = replace(
				ObsYear, 
				which(
					(Season 	== "Spring" | 
					Season 	== "Summer" | 
					Season 	== "Fall") &
					Year 	== 2010 
				), 
				"Year2010"
			),
			ObsYear = replace(
				ObsYear, 
				which(
					(Season 	== "Spring" | 
					Season 	== "Summer" | 
					Season 	== "Fall") &
					Year 	== 2011
				), 
				"Year2011"
			),
			ObsYear = replace(
				ObsYear, 
				which(
					(Season 	== "Spring" | 
					Season 	== "Summer" | 
					Season 	== "Fall") &
					Year 	== 2012
				), 
				"Year2012"
			),
			ObsYear = replace(
				ObsYear, 
				which(
					(Season 	== "Spring" | 
					Season 	== "Summer" | 
					Season 	== "Fall") &
					Year 	== 2013
				), 
				"Year2013"
			),
			ObsYear = replace(
				ObsYear, 
				which(
					Date 	>= "2009-12-21" &
					Date 	< "2010-3-20"
				), 
				"Year2009"
			),
			ObsYear = replace(
				ObsYear, 
				which(
					Date 	>= "2010-12-21" &
					Date 	< "2011-3-20"
				), 
				"Year2010"
			),
			ObsYear = replace(
				ObsYear, 
				which(
					Date 	>= "2011-12-22" &
					Date 	< "2012-3-20"
				), 
				"Year2011"
			),
			ObsYear = replace(
				ObsYear, 
				which(
					Date 	>= "2012-12-21" &
					Date 	< "2013-3-20"
				), 
				"Year2012"
			),
			ObsYear = replace(
				ObsYear, 
				which(
					Date 	>= "2013-12-21" &
					Date 	< "2014-3-20"
				), 
				"Year2013"
			)
		) %>%
		ungroup %>%
		group_by(PlantID, ObsYear) %>%
		summarise(
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