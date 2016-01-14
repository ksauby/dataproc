#' Merge Plant Info and Plant Surveys data files
#'
#' @description Add Island, Cluster, and Host Species information to the Plant_Surveys dataset.
#' @param Plant_Surveys Plant Survey Dataset
#' @param Plant_Info  Plant Information Dataset
#'
#' @export

mergePlantSurveysPlantInfo <- function(Plant_Surveys, Plant_Info) {
	Plant_Surveys <- Plant_Info %>%
		dplyr::select(
			ClusterID, 
			InDemographicStudy, 
			PlotPlantID, 
			Tag_Number, 
			Island, 
			Cluster2, 
			Network, 
			Species, 
			InBigPlantStudy, 
			N.PlotPlantIDs, 
			Easting, 
			Northing
		) %>%
		merge(Plant_Surveys, by = "PlotPlantID")
	return(Plant_Surveys)
}

#' Add Sampling Period Variable
#'
#' @param Plant_Surveys Plant Survey Dataset
#'
#' @export

addSamplingPeriods <- function(Plant_Surveys) {
	Plant_Surveys$DemographicSurvey <- "NA"
	Plant_Surveys %<>% 
		group_by(Date) %>% 
		mutate(
			# SURVEY 1 - SPRING/SUMMER 2013
			DemographicSurvey = replace(DemographicSurvey, 
				which(Date >= "2013-05-14" & Date < "2013-08-06"), "1"),
			# SURVEY 2 - FALL/WINTER 2013/2014
			DemographicSurvey = replace(DemographicSurvey, 
				which(Date >= "2013-12-13" & Date < "2014-01-28"), "2"),
			# SURVEY 3 - SPRING/SUMMER 2014
			DemographicSurvey = replace(DemographicSurvey, 
				which(Date >= "2014-05-06" & Date < "2014-09-24"), "3"),
			# SURVEY 4 - WINTER 2015
			DemographicSurvey = replace(DemographicSurvey, 
				which(Date >= "2015-01-08" & Date < "2015-02-21"), "4"),
			# SURVEY 5 - WINTER 2015
			DemographicSurvey = replace(DemographicSurvey, 
				which(Date >= "2015-05-01"), "5"))
	return(Plant_Surveys)
}

#' Add Sampling Year Variable
#'
#' @param Plant_Surveys Plant Survey Dataset
#'
#' @export

addSamplingYear <- function(Plant_Surveys) {
	Plant_Surveys$Visit <- "NA"
	Plant_Surveys %<>% 
		group_by(Date) %>% 
		mutate(
			Visit = replace(
				Visit, 
				which(Date >= "2012-12-02" & Date < "2013-05-01"), 
				"Year0"
			),
			Visit = replace(
				Visit, 
				which(Date >= "2013-05-01" & Date < "2014-05-01"), 
				"Year1"
			),
			Visit = replace(
				Visit, 
				which(Date >= "2014-05-01" & Date < "2015-05-01"), 
				"Year2"
			),
			Visit = replace(
				Visit, 
				which(Date >= "2015-05-01"), 
				"Year3"
			)
		)
	return(Plant_Surveys)
}