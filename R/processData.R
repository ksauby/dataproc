#' Process dataset
#' 
#' @param dataset Dataset
#' @description Process data:
#' \itemize{
#'  \item rename locations, variables, and species
#'  \item format variables
#'  \item process date and check for duplicate entries
#'  \item create "Visit Number" field
#'  \item create "Fruit Presence" ariable
#'  \item Classify sites as coastal or inland
#'  \item Create variables for insect presence during the entire study and during the current study
#'  \item Create season variable
#'  \item Classify surveys as complete
#'  \item calculate plant volume
#' 	\item create PlantID2
#' }
#'
#' @export
#' @importFrom zoo as.yearmon

processData <- function(dataset) {
	# read in data
	timeseries <- dataset %>% as.data.table
	# rename Mexico Beach and Sweetwater
	timeseries[Location == "MexicoBeach", 	Location := "MB"]
	timeseries[Location == "Sweetwater", 	Location := "TSP"]
	timeseries[Location == "Nokuse",	 	Location := "NP"]
	# create unique PlantID variable
	timeseries %<>% mutate(PlantID = paste(Location, ID, sep=""))
	# remame column namesnames
	timeseries %<>%
		setnames("Height", "Height_t") %>%
		setnames("Pads", "Size_t") %>%
		setnames("MEPR", "ME_t") %>%
		setnames("CACA", "CA_t") %>%
		setnames("DACT", "DA_t") %>%
		setnames("CHVI", "CH_t") %>%
		setnames("Width", "Width_t") %>%
		setnames("Fruit", "Fruit_t")
	# Rename Species factor labels
	timeseries %<>% as.data.table
	timeseries[Species == "humifusa", Species := "Opuntia humifusa"]
	timeseries[Species == "stricta", Species := "Opuntia stricta"]
	# format as factor
	timeseries$Location %<>% as.factor
	timeseries$Species %<>% as.factor
	# Change Zeros to NAs
		# timeseries[timeseries$PlantSize_t==0, ]
		# timeseries[timeseries$PlantHeight_t==0, ]
		# timeseries[timeseries$Width_t==0, ]

		# There should never be observations of 0 pads, 0 height, or 0 width
		# replace 0 with NA
	cnames <- c("Size_t", "Height_t", "Width_t")
	for (cname in cnames) {
		timeseries[, cname := Zero_is_NA_Function(timeseries[[cname]]), with=FALSE]
	}
	############################################################################
	# Process TIME
	############################################################################
	timeseries$Date %<>% as.Date("%m/%d/%y")
	timeseries %<>% 
		mutate(
			Month 			= month(Date),
			MonthYear 		= as.character(as.yearmon(Date)),
			DaysSinceStart 	= as.numeric(Date - min(Date)),
			VisitNum 		= 0
		)	 
	########################## ERROR MESSAGE
	# check first duplicate data entries
	dups <- timeseries %>% 
		group_by(Location, ID, Date) %>%
		summarise(n.obs = length(Species)) %>%
		filter(n.obs > 1)

	if (dim(dups)[1] > 0) {
			stop("Duplicates observations for a PlantID, Date combination are 
				present in the dataset.")
	}
	#--------------------------------------------------------------------------#
	# create "Visit Number" field
	#--------------------------------------------------------------------------#
	timeseries %<>% as.data.table
	timeseries[Date >= "2009-01-21" & Date <= "2009-01-26", VisitNum := 1]
	timeseries[Date >= "2009-04-25" & Date <= "2009-04-30", VisitNum := 2]
	timeseries[Date >= "2009-07-23" & Date <= "2009-07-28", VisitNum := 3]
	timeseries[Date >= "2009-10-23" & Date <= "2009-10-28", VisitNum := 4]
	timeseries[Date >= "2010-05-14" & Date <= "2010-05-19", VisitNum := 5]
	timeseries[Date >= "2010-11-15" & Date <= "2010-11-20", VisitNum := 6]
	timeseries[Date >= "2011-05-12" & Date <= "2011-05-17", VisitNum := 7]
	timeseries[Date >= "2011-12-15" & Date <= "2012-01-17", VisitNum := 8]
	timeseries[Date >= "2012-05-13" & Date <= "2012-05-16", VisitNum := 9]
	timeseries[Date >= "2012-12-14" & Date <= "2012-12-19", VisitNum := 10]
	timeseries[Date >= "2013-05-18" & Date <= "2013-05-23", VisitNum := 11]
	timeseries[Date >= "2014-01-12" & Date <= "2014-01-17", VisitNum := 12]
	########################## ERROR MESSAGE
	# check for erroneous dates
	date_errors <- filter(timeseries, VisitNum=="0")
	# timeseries <- timeseries[which(timeseries$VisitNum!="0"), ]
	if (dim(date_errors)[1] > 0) {
			stop("At least one observation was given a visit number of 0.")
	}
	############################################################################
	# Fruit Presence
	############################################################################
	# make FruitPres variable
	timeseries %<>% 
		mutate(FruitPres_t = ifelse(Fruit_t>0, 1, 0)) %>%
		as.data.table %>%
		# add NAs
		.[is.na(Fruit_t), FruitPres_t := NA]
	############################################################################
	# Classify sites as coastal or inland
	############################################################################
	timeseries %<>%
		mutate(Coastal = as.integer(NA)) %>%
		as.data.table %>%
		# Nokuse and TSP are given 0 because they are not coastal
		.[Location=="N" | Location=="TSP", Coastal := 0] %>%
		.[Location!="N" & Location!="TSP", Coastal := 1]
	
	timeseries$Coastal %<>% as.factor
	############################################################################
	# Insect presence during entire study
	############################################################################
	#--------------------------------------------------------------------------#
	# INSECT ever present?
	#--------------------------------------------------------------------------#
	timeseries %<>%
		group_by(PlantID) %>%
		mutate(
			CACAPresent = ifelse(sum(CA_t, na.rm=T) > 0, 1, 0),
			MEPRPresent = ifelse(sum(ME_t, na.rm=T) > 0, 1, 0),
			DACTPresent = ifelse(sum(DA_t, na.rm=T) > 0, 1, 0),
			CHVIPresent = ifelse(sum(CH_t, na.rm=T) > 0, 1, 0)
		)
	############################################################################
	# Insect presence during current survey
	############################################################################
	timeseries %<>% as.data.frame
	timeseries$NumInsectSpecies_t <- timeseries %>%
		dplyr::select(ME_t, CA_t, CH_t, DA_t) %>%
		apply(., 1, mysum
	)
	############################################################################
	# SEASON
	############################################################################
	timeseries$Date %<>%
		strptime("%Y-%m-%d") %>%
		as.POSIXct(format="%Y-%m-%d")
	timeseries %<>% as.data.frame %>% assignSeason
	timeseries$Date %<>% as.Date
	############################################################################
	# Classify Surveys as Complete
	############################################################################
	timeseries$complete_insect_surveys <- ifelse(
		rowSums(
			is.na(
				timeseries[, c("ME_t", "CA_t", "CH_t", "DA_t")]
			)
		)==0, 1, 0
	)
	timeseries$complete_surveys <- ifelse(
		rowSums(
			is.na(
				timeseries[, 
					c(
						# insects
						"ME_t",                
						"CA_t",
						"CH_t",
						"DA_t",
						# number of segments
						"Size_t",
						# flowers and fruit
						"Fruit_t"
					)
				]
			)
		)==0, 1, 0
	)
	############################################################################
	# Calculate Volume
	############################################################################
	timeseries %<>% 
		mutate(
			Cone_t = pi * (((Width_t)/4)^2) * Height_t / 3,
			Cylinder_Tall_t = pi * ((Width_t/2)^2) * Height_t
		)
	############################################################################
	# Create PlantID2
	############################################################################
	timeseries$PlantID2 <- factor(timeseries$PlantID,
		levels=c(
			# BLSP humifusa
			"BLSPOH2",
			"BLSPOH4",
			"BLSPOH5",
			"BLSPOH6",
			"BLSPOH7",
			"BLSPOH8",
			"BLSPOH9",  
			"BLSPOH10",
			"BLSPOH11",
			"BLSPOH11B",
			"BLSPOH11C",
			"BLSPOH12",
			"BLSPOH13",
			"BLSPOH14",       
			"BLSPOH15",
			"BLSPOH16",
			"BLSPOH17",
			"BLSPOH18",
			"BLSPOH19",
			"BLSPOH20",       
			# HBSP humifusa
			"HBSPOH1",
			"HBSPOH2",
			"HBSPOH3",
			"HBSPOH4",
			"HBSPOH5",        
			"HBSPOH6",
			"HBSPOH7",
			"HBSPOH8",
			"HBSPOH9",
			"HBSPOH10",
			"HBSPOH11",
			"HBSPOH12",
			"HBSPOH13",
			"HBSPOH14",
			"HBSPOH15",       
			"HBSPOH16",
			"HBSPOH17",
			"HBSPOH18",
			# HBSP stricta
			"HBSPOS1",
			"HBSPOS2",
			"HBSPOS3",
			"HBSPOS4",
			"HBSPOS5",
			"HBSPOS6",
			"HBSPOS6B",
			"HBSPOS7",       
			"HBSPOS8",
			"HBSPOS9",
			"HBSPOS10",
			"HBSPOS11" ,       
			"HBSPOS12",
			"HBSPOS13",
			"HBSPOS14",
			"HBSPOS15",
			"HBSPOS16",
			"HBSPOS17",
			"HBSPOS18" ,   
			# Mexico Beach
			"MBOS1",
			"MBOS4",
			"MBOS5",
			"MBOS6",
			"MBOS7",
			"MBOS10",
			"MBOS11",
			"MBOS12",
			"MBOS13" ,
			"MBOS13B",
			"MBOS14",
			"MBOS15",
			"MBOS16",
			"MBOS17",
			"MBOS18",
			"MBOS18B",
			"MBOS19",
			"MBOS20",
			# N
			"NPOH1",       
			"NPOH1B",
			"NPOH2",
			"NPOH3" ,      
			"NPOH4",
			"NPOH5",
			"NPOH6",
			"NPOH7",
			"NPOH7B",
			"NPOH8",
			"NPOH8B",      
			"NPOH9",
			"NPOH9B",
			"NPOH10",
			"NPOH11",
			"NPOH11B",
			"NPOH12",
			"NPOH13",
			"NPOH14",
			"NPOH14B",     
			"NPOH15",
			"NPOH16",
			"NPOH16B",
			"NPOH17",
			"NPOH17B",
			"NPOH18",
			"NPOH19" ,     
			"NPOH19B",
			"NPOH20",
			"NPOH20B",
			# SASP humifusa
			"SASPOH1",
			"SASPOH2",
			"SASPOH3",
			"SASPOH4",
			"SASPOH5" ,        
			"SASPOH6",
			"SASPOH6B",
			"SASPOH7",
			"SASPOH8",
			"SASPOH9",
			"SASPOH10",
			"SASPOH11",
			"SASPOH11B",
			"SASPOH12"  ,      
			"SASPOH13",
			"SASPOH14",
			"SASPOH15",
			# SASP stricta
			"SASPOS1",
			"SASPOS2",
			"SASPOS3" ,        
			"SASPOS4",
			"SASPOS5",
			"SASPOS6",
			"SASPOS7",
			"SASPOS7B",
			"SASPOS8",
			"SASPOS9" ,        
			"SASPOS9B",
			"SASPOS10" ,       
			"SASPOS11",
			"SASPOS12",
			"SASPOS13",
			"SASPOS14",
			"SASPOS15",
			# TSP
			"TSPOH1",
			"TSPOH1B",
			"TSPOH2"  , 
			"TSPOH3",
			"TSPOH4",
			"TSPOH5",
			"TSPOH6",
			"TSPOH7",
			"TSPOH8" ,  
			"TSPOH9",
			"TSPOH10",
			"TSPOH11",
			"TSPOH12",
			"TSPOH13",
			"TSPOH14" , 
			"TSPOH15",
			"TSPOH16",
			"TSPOH17",
			"TSPOH18",
			"TSPOH19",
			"TSPOH20"
		)
	)	
	levels(timeseries$PlantID2) <- c(
		paste("BLSP", 1:20),
		paste("HBSP", 1:18),
		paste("HBSP", 1:19),
		paste("MB", 1:18),
		paste("NP", 1:30),
		paste("SASP", 1:17),
		paste("SASP", 1:17),
		paste("TSP", 1:21)
	)
	############################################################################
	return(timeseries)
}