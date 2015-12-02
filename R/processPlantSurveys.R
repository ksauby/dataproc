#' Process Plant Survey Data

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
#' 	}
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
	return(Plant_Surveys)
}