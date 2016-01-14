#' Create the Survival Dataset
#' 
#' @param timeseries Dataset
#' @description Steps to the creation of the dataset:
#' \itemize{
#'  \item restrict to plants that were first surveyed during the first survey of the study
#'  \item removed plants subject to management action, including:
#' \itemize{
#'  \item HBSPOS10
#'  \item HBSPOS11
#'  \item HBSPOS12
#'  \item HBSPOS15
#'  \item HBSPOS16
#'  \item HBSPOS17
#'  \item HBSPOS18
#'  \item HBSPOS5
#' }
#'  \item summarize whether plant was every infested with an insect
#'  \item summarize whether a plant died during the study
#' }
#' 
#' @export

createSurvivalDataset <- function(timeseries) {
	# plant IDs in the first survey
	FirstSurveyPlantIDs <- 
		timeseries[which(timeseries$VisitNum==1), ]$PlantID %>% 
		unique
	# filter out plants not surveyed during the first survey; filter out plants hacked by the USDA
	timeseries_survival = timeseries %>%
		as.data.frame %>%
		filter(
			PlantID != "HBSPOS10" &
			PlantID != "HBSPOS11" &
			PlantID != "HBSPOS12" &
			PlantID != "HBSPOS15" &
			PlantID != "HBSPOS16" &
			PlantID != "HBSPOS17" &
			PlantID != "HBSPOS18" &
			PlantID != "HBSPOS5"
		) %>%
		filter(PlantID %in% FirstSurveyPlantIDs)
		# lag_size_fruit_function %>%
		# lag_dates_function %>%
		# lag_insects_function
	# create "Dead_numeric" variable with values 0, 1
	# create "Dead" variable with values "Dead", "Alive"			
	# timeseries_survival$Dead_numeric <- timeseries_survival$Dead
	# timeseries_survival$Dead %<>% 
	#	as.factor %>% 
	#	revalue(c("0"="Alive", "1"="Dead"))
	# summarise whether plant was every infested with an insect
	# summarise whether a plant died during the study
	timeseries_survival %<>%
		group_by(Species, PlantID2) %>%
		summarise(
			CA = ifelse(Sum(CA_t) > 0, 1, 0),
			CH = ifelse(Sum(CH_t) > 0, 1, 0),
			DA = ifelse(Sum(DA_t) > 0, 1, 0),
			ME = ifelse(Sum(ME_t) > 0, 1, 0),
			Dead = 	ifelse(Sum(Dead) > 0, 1, 0),
			Location = Location[1],
			YearDied = ifelse(
				Sum(Dead) > 0,
				max(Year),
				NA
			)
		) %>%
		as.data.frame
	return(timeseries_survival)
}