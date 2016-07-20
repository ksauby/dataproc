#' Process Plant Survey Data
#'
#' @description Steps:
#' \itemize{
#'  \item Fix column names
#'  \item format dates
#'  \item format PlantIDs
#'  \item format convert "999" values to NA
#'  \item format change "yes"/"no" values of the insect survey, missing, and dead columns to 0/1
#'  \item format numeric columns
#'  \item add total segment column
#'  \item add fruit count column
#'  \item add fruit/flower count column
#'  \item check for observations of 0 pads, 0 height, or 0 width; if there are observations, stop
#' 	}
#' Column Names:
#' \itemize{
#'  \item PlantMeasureID Record number if Microsoft Access database
#'  \item First_Observer_Initials Initials of primary observer (should always be KS, for Kristen Sauby)
#'  \item Second_Observer_Initials Initials of secondary observer; CJP - Cory Penca; YP - Yani Paulay; KS - Kristen Sauby; JW: Juliana Welch; CW: Cedric Worman; AP: Adam Payton
#'  \item Date Date
#'  \item PlantID Unique number for the individual plant
#'  \item Plant_collected Were plant samples collected?
#'  \item Time
#'  \item PlantPictures identifying numbers of photos taken
#'  \item PlantPictures_Direction direction that the photo(s) was(were) taken

#'  \item CA_t Presence/absence (1/0) of Cactoblastis cactorum
#'  \item CACA_collected Were samples of Cactoblastis cactorum collected?
#'  \item CACA_quantity Number of Cactoblastis cactorum samples collected

#'  \item ME_t Presence/absence (1/0) of Melitara prodenialis
#'  \item MEPR_collected Were samples of Melitara prodenialis collected?
#'  \item MEPR_quantity Number of Melitara prodenialis samples collected

#'  \item CH_t Presence/absence (1/0) of Chelinidea vittiger
#'  \item CHVI_collected Were samples of Chelinidea vittiger collected?
#'  \item CHVI_quantity Number of Chelinidea vittiger samples collected

#'  \item DA_t Presence/absence (1/0) of Dactylopius species
#'  \item DACT_collected Were samples of Dactylopius species collected?
#'  \item DACT_quantity Number of Dactylopius species samples collected

#'  \item Unknown_Moth_t Presence/absence (1/0) of unknown moth
#'  \item UnknownMoth_collected Were samples of unknown moth collected?
#'  \item UnknownMoth_quantity Number of unknown moth samples collected

#'  \item Old_Moth_Evidence_t Evidence of past moth infestation
#'  \item Old_Moth_Evidence_recent Whether evidence of past moth infestation appears recent or old

#'  \item Fungus Presence/absence (1/0) of plant fungal infection
#'  \item Gerstaeckeria Presence/absence (1/0) of Gerstaeckeria
#'  \item Plant_Segments_total Number of segments
#'  \item Plant_Segments_w_leaves New, green segments with leaves
#'  \item Plant_Segments_wo_leaves Green segments without leaves
#'  \item Plant_Segments_woody number of woody segments/trunks; these segments are entirely brown on the outside      
#'  \item Height_t maximum height in cm
#'  \item Width_t maximum width in cm
#'  \item Perpen_Width width, perpendicular to max width, in cm
#'  \item Num_FlowerBuds Number of flower buds
#'  \item Num_Fruit_red Number of red fruit
#'  \item Num_Fruit_green Number of green fruit
#'  \item Num_Flowers Number of flowers
#'  \item Num_Fruit use this when number is recorded but distinction by color is not made
#'  \item Pollinators
#'  \item Spiders
#'  \item Ants
#'  \item Other_collected_quantity
#'  \item Plant_Notes
#'  \item Insect_Notes        
#'  \item Other_Notes
#'  \item Dead Whether the plant is observed to be dead; 0 or 1
#'  \item Missing
#'  \item OutsideOfPlot "Yes" if plant is no longer in plot
#'  \item PlotPlantID Unique number for the individual plant; if a plant is counted in multiple plots, a letter is appended to the plant ID here (e.g., 9606a) and then removed for analysis
#'  \item Size_t The sum of Plant_Segments_total, Plant_Segments_w_leaves, Plant_Segments_wo_leaves, and Plant_Segments_woody

#'  \item Fruit_t The sum of Num_Fruit_red, Num_Fruit_green, and Num_Fruit
#'  \item Fruit_Flowers_t The sum of Num_FlowerBuds, Num_Flowers, Num_Fruit_red, Num_Fruit_green, and Num_Fruit
#' 	}
#'
#' @export

processPlantSurveys <- function(Plant_Surveys) {
	# ------------------------------------------------------------- CHANGE NAMES
	# remame size and height
	Plant_Surveys %<>%	as.data.table %>%
		setnames("Max_Height", 				"Height_t") %>%
		setnames("Max_Width", 				"Width_t") %>%
		setnames("CACA_Larvae", 			"CA_t") %>%
		setnames("MEPR_Larvae", 			"ME_t") %>%
		setnames("CHVI_Evidence", 			"CH_t") %>%
		setnames("DACT_Evidence", 			"DA_t") %>%
		setnames("UnknownMoth_Evidence", 	"Unknown_Moth_t") %>%
		setnames("Old_Moth_Evidence", 		"Old_Moth_Evidence_t") %>%
		setnames("DateSurveyed", 			"Date") %>%
		as.data.frame
	# formatting/preparation necessary for prepping Demographic Plant Info
	Plant_Surveys$Date %<>% Format_Date_Function
	Plant_Surveys %<>% arrange(Date)
	Plant_Surveys %<>% Format_PlantIDs_Function
	# ------------------------------------------------ CONVERT ALL "999s" to NAs
	Plant_Surveys[,c(
		"Plant_Segments_total", 
		"Plant_Segments_w_leaves",
		"Plant_Segments_wo_leaves",
		"Plant_Segments_woody",
		"Perpen_Width",
		"Width_t",
		"Height_t",
		"Num_FlowerBuds",
		"Num_Fruit_red",
		"Num_Fruit_green",
		"Num_Flowers",
		"Num_Fruit")] %<>% 
		apply(2, NA_Function
	)
	# ------------------- INSECT SURVEYS, MISSING, DEAD - CHANGE YES, NO to 0, 1
	Plant_Surveys[,c(
		"CA_t",
		"ME_t",
		"CH_t",
		"DA_t",
		"Unknown_Moth_t",
		"Old_Moth_Evidence_t",
		"Dead",
		"Missing")] %<>% 
		apply(2, Yes_Function
	)
	Plant_Surveys[,c(
		"CA_t",
		"ME_t",
		"CH_t",
		"DA_t",
		"Unknown_Moth_t",
		"Old_Moth_Evidence_t",
		"Dead",
		"Missing")] %<>%
		apply(2, No_Function
	)
	Plant_Surveys[,c(
		"CA_t",
		"ME_t",
		"CH_t",
		"DA_t",
		"Unknown_Moth_t",
		"Old_Moth_Evidence_t",
		"Dead",
		"Missing")] %<>% 
		apply(2, NA_Function
	)
	# ------------------------------------------------------------- MAKE NUMERIC
	Plant_Surveys[,c(
		"CA_t",
		"ME_t",
		"CH_t",
		"DA_t",
		"Unknown_Moth_t",
		"Old_Moth_Evidence_t",
		"Plant_Segments_total",
		"Plant_Segments_w_leaves",
		"Plant_Segments_wo_leaves",
		"Plant_Segments_woody",
		"Height_t",
		"Width_t",
		"Perpen_Width",
		"Num_FlowerBuds",
		"Num_Fruit_red",
		"Num_Fruit_green",
		"Num_Flowers",
		"Num_Fruit",
		"Dead",
		"Missing")] %<>% 
		apply(2, as.numeric
	)
	# ------------------------------------------------- ADD TOTAL SEGMENT COLUMN
	# do this so that plants that have no segments recorded (all NAs) have a total segment count = NA
	# for those plants that have fewer than four NAs (at least one segment column has a number), sum the segments
	Plant_Surveys$Size_t <- Plant_Surveys %>%
							dplyr::select(
								Plant_Segments_total,
								Plant_Segments_w_leaves,
								Plant_Segments_wo_leaves,
								Plant_Segments_woody
							) %>%
							apply(1, mysum)
	Plant_Surveys$Size_t %<>% Zero_is_NA_Function
	# --------------------------------------------------------- ADD FRUIT COLUMN
	Plant_Surveys$Fruit_t <- Plant_Surveys %>%
							dplyr::select(
								Num_Fruit_red,
								Num_Fruit_green,
								Num_Fruit
							) %>%
							apply(1, mysum)
	Plant_Surveys$Fruit_Flowers_t <- Plant_Surveys %>%
							dplyr::select(
								Num_FlowerBuds,
								Num_Flowers,
								Num_Fruit_red,
								Num_Fruit_green,
								Num_Fruit
							) %>%
							apply(1, mysum)
	# ----------------------------------------------------------- ERROR MESSAGES
	# There should never be observations of 0 pads, 0 height, or 0 width
	# replace 0 with NA
	dups <- Plant_Surveys %>% filter(Size_t==0)
	if (dim(dups)[1] > 0) {stop("Values for Size_t equal 0.")}

	dups <- Plant_Surveys %>% filter(Height_t==0)
	if (dim(dups)[1] > 0) {stop("Values for Height_t equal 0.")}

	dups <- Plant_Surveys %>% filter(Width_t==0)
	if (dim(dups)[1] > 0) {stop("Values for Width_t equal 0.")}

	dups <- Plant_Surveys %>% filter(Perpen_Width==0)
	if (dim(dups)[1] > 0) {stop("Values for Perpen_Width equal 0.")}
	return(Plant_Surveys)
}

#' Process Original Plant Survey Data
#'
#' @description I collected this data in December 2012, then gave the plants new Plant IDs. This data cannot be used for the demography study but can be used to determine plot occupancy.
#' Steps:
#' \itemize{
#'  \item Fix column names
#'  \item format dates
#'  \item format PlantIDs
#'  \item format convert "999" values to NA
#'  \item format change "yes"/"no" values of the insect survey, missing, and dead columns to 0/1
#'  \item format numeric columns
#'  \item check for observations of 0 pads, 0 height, or 0 width; if there are observations, stop
#' 	}

#' 	}
#'
#' @export

processOriginalPlantData <- function(Original_Plant_Data) {
	# ------------------------------------------------------------- CHANGE NAMES
	# remame size and height
	Original_Plant_Data %<>% as.data.table %>%
		setnames("Max_Height", 				"Height_t") %>%
		setnames("Max_Width", 				"Width_t") %>%
		setnames("CACA_Larvae", 			"CA_t") %>%
		setnames("MEPR_Larvae", 			"ME_t") %>%
		setnames("CHVI_Evidence", 			"CH_t") %>%
		setnames("DACT_Evidence", 			"DA_t") %>%
		setnames("UnknownMoth_Evidence", 	"Unknown_Moth_t") %>%
		setnames("Old_Moth_Evidence", 		"Old_Moth_Evidence_t") %>%
		as.data.frame
	# formatting/preparation necessary for prepping Demographic Plant Info
	Original_Plant_Data$Date %<>% Format_Date_Function
	Original_Plant_Data %<>% arrange(Date)
	# ------------------------------------------------ CONVERT ALL "999s" to NAs
	Original_Plant_Data[,c(
		"Plant_Segments_w_leaves",
		"Plant_Segments_wo_Leaves",
		"Perpen_Width",
		"Width_t",
		"Height_t",
		"Num_FlowerBuds",
		"Num_Fruit_red",
		"Num_Fruit_green",
		"Num_Flowers",
		"Num_Fruit")] %<>% 
		apply(2, NA_Function
	)
	# ------------------- INSECT SURVEYS, MISSING, DEAD - CHANGE YES, NO to 0, 1
	Original_Plant_Data[,c(
		"CA_t",
		"ME_t",
		"CH_t",
		"DA_t",
		"Unknown_Moth_t",
		"Old_Moth_Evidence_t")] %<>% 
		apply(2, Yes_Function
	)
	Original_Plant_Data[,c(
		"CA_t",
		"ME_t",
		"CH_t",
		"DA_t",
		"Unknown_Moth_t",
		"Old_Moth_Evidence_t")] %<>%
		apply(2, No_Function
	)
	Original_Plant_Data[,c(
		"CA_t",
		"ME_t",
		"CH_t",
		"DA_t",
		"Unknown_Moth_t",
		"Old_Moth_Evidence_t")] %<>% 
		apply(2, NA_Function
	)
	# ------------------------------------------------------------- MAKE NUMERIC
	Original_Plant_Data[,c(
		"CA_t",
		"ME_t",
		"CH_t",
		"DA_t",
		"Unknown_Moth_t",
		"Old_Moth_Evidence_t",
		"Plant_Segments_w_leaves",
		"Plant_Segments_wo_Leaves",
		"Height_t",
		"Width_t",
		"Perpen_Width",
		"Num_FlowerBuds",
		"Num_Fruit_red",
		"Num_Fruit_green",
		"Num_Flowers",
		"Num_Fruit")] %<>% 
		apply(2, as.numeric
	)
	# ----------------------------------------------------------- ERROR MESSAGES
	# There should never be observations of 0 pads, 0 height, or 0 width
	# replace 0 with NA
	dups <- Original_Plant_Data %>% filter(Height_t==0)
	if (dim(dups)[1] > 0) {stop("Values for Height_t equal 0.")}

	dups <- Original_Plant_Data %>% filter(Width_t==0)
	if (dim(dups)[1] > 0) {stop("Values for Width_t equal 0.")}

	dups <- Original_Plant_Data %>% filter(Perpen_Width==0)
	if (dim(dups)[1] > 0) {stop("Values for Perpen_Width equal 0.")}
	return(Original_Plant_Data)
}


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
			Northing,
			ReproductiveMode
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
			# SURVEY 0
			DemographicSurvey = replace(DemographicSurvey, 
				which(Date < "2013-05-14"), "0"),
			# SURVEY 1 - SPRING/SUMMER 2013
			DemographicSurvey = replace(DemographicSurvey, 
				which(Date >= "2013-05-14" & Date <= "2013-08-06"), "1"),
			# SURVEY 2 - FALL/WINTER 2013/2014
			DemographicSurvey = replace(DemographicSurvey, 
				which(Date >= "2013-12-13" & Date <= "2014-01-28"), "2"),
			# SURVEY 3 - SPRING/SUMMER 2014
			DemographicSurvey = replace(DemographicSurvey, 
				which(Date >= "2014-05-06" & Date <= "2014-09-24"), "3"),
			# SURVEY 4 - WINTER 2015
			DemographicSurvey = replace(DemographicSurvey, 
				which(Date >= "2015-01-08" & Date <= "2015-02-21"), "4"),
			# SURVEY 5 - SPRING/SUMMER 2015
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
	Plant_Surveys$SamplingYear <- NA
	Plant_Surveys %<>% 
		group_by(Date) %>% 
		mutate(
			SamplingYear = replace(
				SamplingYear, 
				which(Date >= "2012-12-02" & Date < "2013-05-01"), 
				2012
			),
			SamplingYear = replace(
				SamplingYear, 
				which(Date >= "2013-05-01" & Date < "2014-05-01"), 
				2013
			),
			SamplingYear = replace(
				SamplingYear, 
				which(Date >= "2014-05-01" & Date < "2015-05-01"), 
				2014
			),
			SamplingYear = replace(
				SamplingYear, 
				which(Date >= "2015-05-01"), 
				2015
			)
		)
	return(Plant_Surveys)
}

#' Determine Fruit and Flower Presence
#'
#' @description Create separate variables indicating whether fruit and fruit and flowers were present.
#' @param Plant_Surveys Plant Survey Dataset
#'
#' @export

calculateFruitPresence <- function(Plant_Surveys) {
	Plant_Surveys %>% 
		mutate(
			FruitPres_t 		= ifelse(Fruit_t > 0, 1, 0),
			FruitFlowerPres_t 	= ifelse(Fruit_Flowers_t > 0, 1, 0)
		) %>%
		as.data.table %>%
		# add NAs
		.[is.na(Fruit_t), FruitPres_t := NA] %>%
		.[is.na(Fruit_Flowers_t), FruitPres_t := NA] %>%
		as.data.frame
}

#' Calculate Plant Volume
#'
#' @description Calculate plant volume as a cone, cylinder, and elliptic cylinder.
#' @param Plant_Surveys Plant_Surveys Plant Survey Dataset
#'
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
#'
#' @param x Plant_Surveys Plant Survey Dataset
#'
#' @export

Complete_Surveys_function <- function(x){
	ifelse(rowSums(is.na(x))==0, 1, 0)
	return(x)
}

#' Determine whether a survey was complete
#'
#' @description Determine whether all data was collected during a survey.
#' Determine complete surveys for:
#' \itemize{
#'  \item insect surveys
#'  \item plant size
#'  \item size measurements (cm)
#'  \item fruit, flowers, and flower buds
#'  \item all data
#' 	}
#' @param Plant_Surveys Plant Survey Dataset
#'
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
#'
#' @description Determine if an insect species was ever detected during the study period.
#' @param Plant_Surveys Plant Survey Dataset
#'
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

#' Rename Species levels
#'
#' @param Plant_Surveys Plant Survey Dataset
#'
#' @export

renameSpecies <- function(Plant_Surveys) {
	Plant_Surveys %>% 
		as.data.table %>%
		.[Species == "pusilla", Species := "Opuntia pusilla"] %>%
		.[Species == "stricta", Species := "Opuntia stricta"] %>%
		as.data.frame
}

#' Rename Convert 1/0 to Yes/No
#'
#' @param x Vector of 0/1 data
#'
#' @export

Yes_No_from_1_0_Function <- function(x){
	x[x > 0] <- "Yes"
	x[x == 0] <- "No"
	return(x)
}

#' Make Insect Factor Variables
#'
#' @description Make new insect variables with "yes"/"no" values instead of 0/1.
#' @param Plant_Surveys Plant Survey Dataset
#'
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
#'
#' @description Format variables as factors.
#' @param Plant_Surveys Plant Survey Dataset
#'
#' @export

formatasFactors <- function(x) {
	factors <- c("ClusterID", "Network", "Island", "Species", 
		"DemographicSurvey", "Year", "Season", "C_cactorum", 
		"M_prodenialis")
	for (i in 1:length(factors)) {
		if (factors[i] %in% names(x)) {
			x[, factors[i]] %<>% as.factor
		}
	}
	return(x)
}

#' Process Plant Survey Data
#'
#' @description Steps:
#' \itemize{
#'  \item add column, "DemographicSurvey"
#'	\itemize{
#'		\item survey 1 - spring/summer 2013
#'		\item survey 2 - fall/winter 2013/2014
#'		\item survey 3 - spring/summer 2014
#'		\item survey 4 - winter 2015
#'		\item survey 5 - spring/summer 2015
#'	}
#'  \item addSamplingYear
#'	\itemize{
#'		\item 2012 - Date >= "2012-12-02" & Date < "2013-05-01"
#'		\item 2013 - Date >= "2013-05-01" & Date < "2014-05-01"
#'		\item 2014 - Date >= "2014-05-01" & Date < "2015-05-01"
#'		\item 2015 - Date >= "2015-05-01"
#'	}
#' }
#'
#' @export

processPlantSurveysafterMergewPlantInfo <- function(Plant_Surveys) {
	Plant_Surveys %>%
		filter(InBigPlantStudy!="yes" & InBigPlantStudy!="Yes") %>%
		addSamplingPeriods %>%
		addSamplingYear %>%
		as.data.frame
}
	
#' Process Plant Survey Data, per plant
#'
#' @description Steps:
#' \itemize{
#'  	\item merge records for the same plant from multiple plots; the data to be merged must have been collected on the same day.
#'  	\item Create separate variables indicating whether fruit and fruit and flowers were present
#'  	\item Calculate plant volume as a cone, cylinder, and elliptic cylinder
#'  	\item Determine if an insect species was ever detected during the study period
#'  	\item Rename species levels ("Opuntia stricta" instead of "stricta" and "Opuntia pusilla" instead of "pusilla")
#' 	 	\item Make new insect variables with "yes"/"no" values instead of 0/1, named "C_cactorum" and "M_prodenialis"
#'  	\item format the variables "ClusterID", "Network", "Island", "Species", "DemographicSurvey", "SamplingYear", "Year", "Season", "C_cactorum", "M_prodenialis" as factors
#' 	}
#'
#' @export
	
processSurveysMergedbyPlant <- function(Plant_Surveys) {
	Plant_Surveys %>%
		mergePlantRecordsfromMultiplePlots %>%
		calculateFruitPresence %>%
		calculatePlantVolume %>%
		determineInsectPresenceDuringStudy %>%
		renameSpecies %>%
		createInsectFactorVariables %>%
		formatasFactors
}