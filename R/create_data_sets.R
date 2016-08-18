#' Create Dataset with All Surveys
#' 
#' @description Create dataset from all surveys and calculate:
#' \itemize{
#'  \item lagged fruit values
#'  \item lagged dates
#'  \item lagged insect presence
#'  \item relative growth rate
#' }
#' @param timeseries Dataset
#' 
#' @export

createAllSurveysDataset <- function(timeseries) {
	timeseries_all_surveys <- timeseries
	timeseries_all_surveys %<>% 
		# lag variables
		calculateSizeLags(
			arrange.variable="Date", 
			grouping.variable="PlantID"
		) %>%
		calculateDateLags %>%
		createNewInsectVariables(
			arrange.variable="Date", 
			grouping.variable="PlantID"
		) %>%
		calculateInsectLags(
			arrange.variable="Date", 
			grouping.variable="PlantID"
		) %>%
		# RGR
		calculateRGR
		# convert to factor
		timeseries_all_surveys[,c(
			"Location",
			"Species",
			"Coastal",
			"Season",
			"PlantID",
			"PlantID2")] %<>%
			apply(., 2, as.factor
		)
		# convert to numeric
		timeseries_all_surveys[,c(
			"Cylinder_Tall_t",
			"Cone_t",
			"Height_t",            
			"Width_t",               
			"Height_t_1",
			"Cone_t_1",               
			"Cylinder_Tall_t_1",
			"RGR_Height",
			"RGR_Height365",
			"RGR_Size",
			"RGR_Size365",
			"RGR_Cone",
			"RGR_Cone365",
			"RGR_Cylinder_Tall",
			"RGR_CylinderTall365")] %<>%
			apply(., 2, as.numeric
		)
		# convert to integer
		timeseries_all_surveys[,c(
			"DA_t",
			"CH_t",
			"ME_t",
			"CA_t",
			"Size_t",
			"Fruit_t",  
			"FruitPres_t",
			"CACAPresent",
			"MEPRPresent",
			"DACTPresent",          
			"CHVIPresent",
			"NumInsectSpecies_t",
			"Size_t_1",
			"CA_t_1",
			"ME_t_1",
			"CH_t_1",                 
			"DA_t_1",
			"Dead",
			"DaysSincePrevSurvey")] %<>%
			apply(., 2, as.integer
		)
   		# convert to Date
		timeseries_all_surveys$Date %<>% as.Date(format = "%Y-%m-%d")	
		timeseries_all_surveys$Previous_Survey_Date %<>% 
			as.Date(format = "%Y-%m-%d")	
	# Save
	setwd("/Users/KSauby/Documents/Projects/marsico-time-series/")
	cache("timeseries_all_surveys")
	return(timeseries_all_surveys)
}	

#' Create Create a Fecundity Year Variable
#' 
#' @description Each starts on the first day of spring of that calendar year, then ends on the last day of winter in the next calendar year (e.g., Spring 2009 - Winter 2010).
#' The years are defined as follows:
#' \itemize{
#'  \item Year 2009 - Spring, Summer, Fall, Winter 2009 - 2010
#'  \item Year 2010 - Spring, Summer, Fall, Winter 2010 - 2011
#'  \item Year 2011 - Spring, Summer, Fall, Winter 2011 - 2012
#'  \item Year 2012 - Spring, Summer, Fall, Winter 2012 - 2013
#'	\item Year 2013 - Spring, Summer, Fall, Winter 2013 - 2014
#' }
#' @param timeseries Dataset
#' 
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
					(Season == "Spring" | 
					Season 	== "Summer" | 
					Season 	== "Fall") &
					Year 	== 2011
				), 
				2011
			),
			ObsYear = replace(
				ObsYear, 
				which(
					(Season == "Spring" | 
					Season 	== "Summer" | 
					Season 	== "Fall") &
					Year 	== 2012
				), 
				2012
			),
			ObsYear = replace(
				ObsYear, 
				which(
					(Season == "Spring" | 
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
#' 
#' @param timeseries Dataset
#' @description Create a dataset with yearly observations of fruit, size, and insect observations. Each starts on the first day of spring of that calendar year, then ends on the last day of winter in the next calendar year (e.g., Spring 2009 - Winter 2010). Variables that are calculated included the maximum number of fruit observed, insect presence/absence during the year, and the maximum and minimum plant size that year. All years, except for 2009, have two observations per year.
#' The dataset is created according to the following steps:
#' \itemize{
#'  \item create a year variable (called "ObsYear")
#'  \item determine if insect was ever observed during the ObsYear
#'  \item calculate maximum and minimum plant size and volume
#'  \item determine if surveys were complete for the entire year (no missing information)
#' }
#' 
#' @export

createFruitYearDataset <- function(timeseries) {
	timeseries_fruityear <- timeseries
	timeseries_fruityear %<>% 
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
			ID						= ID[1],
			Fruit_t 				= max(Fruit_t),
			FruitPres_t 			= max(FruitPres_t),
			ME_t 					= max(ME_t),
			CA_t 					= max(CA_t),
			CH_t 					= max(CH_t),
			DA_t 					= max(DA_t),
			Size_max_t 				= max(Size_t),
			Cone_max_t 				= max(Cone_t), 
			Cylinder_Tall_max_t 	= max(Cylinder_Tall_t),
			Size_min_t 				= min(Size_t),
			Cone_min_t 				= min(Cone_t), 
			Cylinder_Tall_min_t 	= min(Cylinder_Tall_t),
			complete_insect_surveys = min(complete_insect_surveys), 
			complete_surveys		= min(complete_surveys)
		) %>%
		createNewInsectVariables(
			arrange.variable="ObsYear", 
			grouping.variable="PlantID"
		) %>%
		# lag variables
		calculateSizeLags(
			arrange.variable="ObsYear", 
			grouping.variable="PlantID"
		) %>%
		calculateFruitLags(
			arrange.variable="ObsYear", 
			grouping.variable="PlantID"
		) %>%
		calculateInsectLags(
			arrange.variable="ObsYear", 
			grouping.variable="PlantID"
		)
}