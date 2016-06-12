#' Remove Data
#' 
#' @description Remove from the dataset:
#' \itemize{
#'  \item any O. engelmannii
#'  \item observations of the following plants after 2011-05-16:
#' 		\itemize{
#' 			\item HBSPOS10
#' 			\item HBSPOS11
#' 			\item HBSPOS12
#' 			\item HBSPOS15
#' 			\item HBSPOS16
#' 			\item HBSPOS17
#' 			\item HBSPOS18
#' 			\item HBSPOS5
#' 		}
#'  \item all observations of the following plants:
#' 		\itemize{
#' 			\item MBOS3
#' 			\item BLSPOH1
#' 			\item BLSPOH3
#' 			\item NOH1A
#' 		}
#'  \item duplicate observations of dead plants
#' }
#' @param timeseries Dataset of plant surveys
#'
#' @export

removeData <- function(timeseries) {
	timeseries %<>% as.data.frame %>%
		# not enough engelmannii observations for analysis
		filter(Species!="engelmannii") %>% 
		# remove surveys of these plants that occurred after 2011-05-16 because they were hacked by the USDA (this was discovered at HBSP on 5/17/11)
		filter(!(PlantID == "HBSPOS10" & Date > "2011-05-16")) %>%
		filter(!(PlantID == "HBSPOS11" & Date > "2011-05-16")) %>%
		filter(!(PlantID == "HBSPOS12" & Date > "2011-05-16")) %>%
		filter(!(PlantID == "HBSPOS15" & Date > "2011-05-16")) %>%
		filter(!(PlantID == "HBSPOS16" & Date > "2011-05-16")) %>%
		filter(!(PlantID == "HBSPOS17" & Date > "2011-05-16")) %>%
		filter(!(PlantID == "HBSPOS18" & Date > "2011-05-16")) %>%
		filter(!(PlantID == "HBSPOS5" & Date > "2011-05-16")) %>%
		# Remove Mexico Beach OS3
		# 		I don't know if this is always the same plant being surveyed
		filter(!(PlantID == "MBOS3")) %>%
		# these were multiple plants counted as one
		filter(!(PlantID == "BLSPOH1")) %>%
		filter(!(PlantID == "BLSPOH3")) %>%
		# not sure if this was from parent OH1 or not
		filter(!(PlantID == "NPOH1A"))
	#----------------------------------------------------------------------#
	# Keep only first observation that a plant is dead
	timeseries_not_dead <- timeseries %>%
		filter(Dead==0)
	timeseries_dead <- timeseries %>%
		filter(Dead==1) %>%
		arrange(Date) %>%
		# keep only the first record that the plant was dead
		ddply("PlantID", function(z) head(z,1))
	########################## ERROR MESSAGE
	# doublecheck that there is only one row per PlantID in timeseries_dead
	if (length(unique(timeseries_dead$PlantID))!=dim(timeseries_dead)[1]) {
			stop("There is more than one row per PlantID in timeseries_dead.")
	}
	##########################
	# make all other fields NA so surveys when dead don't end up in an analysis
	timeseries_dead$Height_t <- NA
	timeseries_dead$Width_t <- NA
	timeseries_dead$Size_t <- NA
	timeseries_dead$Fruit_t <- NA
	timeseries_dead$ME_t <- NA
	timeseries_dead$CA_t <- NA
	timeseries_dead$CH_t <- NA
	timeseries_dead$DA_t <- NA
	timeseries <- rbind(timeseries_not_dead, timeseries_dead)
	return(timeseries)
}

#' Keep USDA data
#' 
#' @description Save data on USDA managed plants including observations of the following plants after 2010-10-01 (first observed management by USDA on 2011-05-16):
#' \itemize{
#' 		\item HBSPOS10
#' 		\item HBSPOS11
#' 		\item HBSPOS12
#' 		\item HBSPOS15
#' 		\item HBSPOS16
#' 		\item HBSPOS17
#' 		\item HBSPOS18
#' 		\item HBSPOS5
#' }
#' @param timeseries Dataset of plant surveys
#'
#' @export

keepUSDAData <- function(timeseries) {
	timeseries %<>% as.data.frame %>%
		# not enough engelmannii observations for analysis
		# remove surveys of these plants that occurred after 2011-05-16 because they were hacked by the USDA
		# keep dates before USDA hacking so RGR can be calculated
		filter( 
			(PlantID == "HBSPOS10" & Date > "2010-10-01") |
			(PlantID == "HBSPOS11" & Date > "2010-10-01") |
			(PlantID == "HBSPOS12" & Date > "2010-10-01") |
			(PlantID == "HBSPOS15" & Date > "2010-10-01") |
			(PlantID == "HBSPOS16" & Date > "2010-10-01") |
			(PlantID == "HBSPOS17" & Date > "2010-10-01") |
			(PlantID == "HBSPOS18" & Date > "2010-10-01") |
			(PlantID == "HBSPOS5" & Date > "2010-10-01") 
		) 
	return(timeseries)
}