#' Determine Fruit and Flower Presence
#' @description Create separate variables indicating whether fruit and fruit and flowers were present.
#' @param Plant_Surveys
#' @export

calculateFruitPresence <- function(Plant_Surveys) {
	Plant_Surveys %>% 
		mutate(
			FruitPres_t 		= ifelse(Fruit_t > 0, 1, 0),
			FruitFlowerPres_t 	= ifelse(Fruit_Flowers_t > 0, 1, 0)
		)
}

#' Calculate Plant Volume
#' @description Calculate plant volume as a cone, cylinder, and elliptic cylinder.
#' @param Plant_Surveys
#' @export

calculatePlantVolume <- function(Plant_Surveys) {
	Plant_Surveys %>% 
		mutate(
			Cone_t 				= pi * (((Width_t + Perpen_Width)/4)^2) * 
									Height_t / 3,
			Cylinder_t 			= pi * ((Perpen_Width/2)^2) * Width_t,
			Elliptic_Cylinder_t = pi * Height_t/2 * Perpen_Width/2 * Width_t
		)
}

#' Determine if a survey was complete
#' @param x
#' @export

Complete_Surveys_function <- function(x){
	ifelse(rowSums(is.na(x))==0, 1, 0)
	return(x)
}

#' Determine whether a survey was complete
#' @description Determine whether all data was collected during a survey.
#' Determine complete surveys for:
#' \itemize{
#'  \item insect surveys
#'  \item plant size
#'  \item size measurements (cm)
#'  \item fruit, flowers, and flower buds
#'  \item all data
#' 	}
#' @param Plant_Surveys
#' @export

determineCompleteSurveys <- function(Plant_Surveys) {
	Plant_Surveys$complete_insect_surveys <- ifelse(rowSums(is.na(
		Plant_Surveys[, 
		c(
			"CA_t",
			"ME_t",
			"Unknown_Moth_t",
			"Old_Moth_Evidence_t"
		)]))==0, 1, 0
	)
	Plant_Surveys$complete_segments_surveys <- ifelse(rowSums(is.na(
		Plant_Surveys[, 
		c(
			"Plant_Segments_w_leaves",
			"Plant_Segments_wo_leaves",
			"Plant_Segments_woody"
		)]))==0, 1, 0
	)
	Plant_Surveys$complete_size_surveys <- ifelse(rowSums(is.na(
		Plant_Surveys[, 
		c(
			"Height_t",
			"Width_t",
			"Perpen_Width"
		)]))==0, 1, 0
	)
	Plant_Surveys$complete_fruit_surveys <- ifelse(rowSums(is.na(
		Plant_Surveys[, 
		c(
			"Num_FlowerBuds",
			"Num_Fruit_red",
			"Num_Fruit_green",
			"Num_Flowers"
		)]))==0, 1, 0
	)
	Plant_Surveys$complete_surveys <- ifelse(rowSums(is.na(
		Plant_Surveys[, 
		c(
		# insects
		"CA_t",
		"ME_t",
		"Unknown_Moth_t",
		"Old_Moth_Evidence_t",
		# number of segments
		"Size_t",
		"Plant_Segments_w_leaves",
		"Plant_Segments_wo_leaves",
		"Plant_Segments_woody",
		# size in cm
		"Height_t",
		"Width_t",
		"Perpen_Width",
		# flowers and fruit
		"Fruit_t" 
		)]))==0, 1, 0
	)
	return(Plant_Surveys)
}

#' Determine if an insect species was ever detected during the study period
#' @description Determine if an insect species was ever detected during the study period.
#' @param Plant_Surveys
#' @export

determineInsectPresenceDuringStudy <- function(Plant_Surveys) {
	Plant_Surveys.present <- Plant_Surveys %>%
		group_by(PlantID) %>%
		summarise(
			CAPresent = ifelse(sum(CA_t, na.rm=T) > 0, 1, 0),
			MEPresent = ifelse(sum(ME_t, na.rm=T) > 0, 1, 0)
			) %>%
		as.data.frame()
	Plant_Surveys %>% merge(Plant_Surveys.present, by="PlantID")
}

#' Assign the appropriate season for each sampling date
#' @param Plant_Surveys
#' @export

assignSeason <- function(Plant_Surveys) {
	Plant_Surveys$Year = year(Plant_Surveys$Date)
	Plant_Surveys$Season = NA
	Plant_Surveys[which(Plant_Surveys$Year==2012),]$Season <- 
		getSeason2012(Plant_Surveys[which(Plant_Surveys$Year==2012),]$Date)
	Plant_Surveys[which(Plant_Surveys$Year==2013),]$Season <- 
		getSeason2013(Plant_Surveys[which(Plant_Surveys$Year==2013),]$Date)
	Plant_Surveys[which(Plant_Surveys$Year==2014),]$Season <- 
		getSeason2014(Plant_Surveys[which(Plant_Surveys$Year==2014),]$Date)
	return(Plant_Surveys)
}

#' Rename Species levels
#' @param Plant_Surveys
#' @export

renameSpecies <- function(Plant_Surveys) {
	Plant_Surveys %>% 
		mutate(
			Species = replace(
				Species, 
				which(Species =="pusilla"), 
				"Opuntia pusilla"
			),
			Species = replace(
				Species, 
				which(Species =="stricta"), 
				"Opuntia stricta"
			)
		)
}

#' Rename Convert 1/0 to Yes/No
#' @param x
#' @export

Yes_No_from_1_0_Function <- function(x){	
	x[x > 0] <- "Yes"
	x[x == 0] <- "No"
	return(x)
}

#' Make Insect Factor Variables
#' @description Make new insect variables with "yes"/"no" values instead of 0/1.
#' @param Plant_Surveys
#' @export

createInsectFactorVariables <- function(Plant_Surveys) {
	Plant_Surveys$C_cactorum 	<- Plant_Surveys$CA_t
	Plant_Surveys$M_prodenialis <- Plant_Surveys$ME_t
	Plant_Surveys[,c(
		"C_cactorum",
		"M_prodenialis")] %<>%
		apply(., 2, Yes_No_from_1_0_Function) %>%
		apply(., 2, as.factor
	)
	return(Plant_Surveys)
}

#' Format as Factors
#' @description Format variables as factors.
#' @param Plant_Surveys
#' @export

formatasFactors <- function(Plant_Surveys) {
	Plant_Surveys$ClusterID 		%<>% as.factor
	Plant_Surveys$Network 			%<>% as.factor
	Plant_Surveys$Island 			%<>% as.factor
	Plant_Surveys$Species 			%<>% as.factor
	Plant_Surveys$DemographicSurvey %<>% as.factor
	Plant_Surveys$Visit 			%<>% as.factor
	Plant_Surveys$Year 				%<>% as.factor
	Plant_Surveys$Season 			%<>% as.factor
	Plant_Surveys$C_cactorum 		%<>% as.factor
	Plant_Surveys$M_prodenialis 	%<>% as.factor
	return(Plant_Surveys)
}

