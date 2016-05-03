#' Abbreviate Variable Names
#'
#' @description Steps:
#' \itemize{
#'  	\item Replace "CACA_on_Pusilla" with "P_Ca"
#'  	\item Replace "MEPR_on_Pusilla" with "P_Me"
#'  	\item Replace "CHVI_on_Pusilla" with "P_Ch"
#'  	\item Replace "UnknwnMoth_on_Pusilla" with "P_Umoth"
#'  	\item Replace "Old_Moth_Evidence_Pusilla" with "P_Omoth"
#'  	\item Replace "Height_Pusilla" with "P_H"
#'  	\item Replace "Percent_Cover_Pusilla" with "P_PC"
#'  	\item Replace "CACA_on_Stricta" with "S_Ca"
#'  	\item Replace "MEPR_on_Stricta" with "S_Me"
#'  	\item Replace "CHVI_on_Stricta" with "S_Ch"
#'  	\item Replace "UnknwnMoth_on_Stricta" with "S_Umoth"
#'  	\item Replace "Old_Moth_Evidence_Stricta" with "S_Omoth"
#'  	\item Replace "Height_Stricta" with "S_H"
#'  	\item Replace "Percent_Cover_Stricta" with "S_PC"
#' 	}
#'
#' @export

changeVariableNames <- function(Plot_Surveys) {
	Plot_Surveys %<>% 
		as.data.table %<>%
		# pusilla
		setnames("CACA_on_Pusilla", 			"P_Ca") %>%
		setnames("MEPR_on_Pusilla", 			"P_Me") %>%
		setnames("CHVI_on_Pusilla", 			"P_Ch") %>%
		setnames("UnknwnMoth_on_Pusilla", 		"P_Umoth") %>%
		setnames("Old_Moth_Evidence_Pusilla", 	"P_Omoth") %>%
		setnames("Height_Pusilla", 				"P_H") %>%
		setnames("Percent_Cover_Pusilla",		"P_PC")  %>%
		# stricta
		setnames("CACA_on_Stricta", 			"S_Ca") %>%
		setnames("MEPR_on_Stricta", 			"S_Me") %>%
		setnames("CHVI_on_Stricta", 			"S_Ch") %>%
		setnames("UnknwnMoth_on_Stricta", 		"S_Umoth") %>%
		setnames("Old_Moth_Evidence_Stricta", 	"S_Omoth") %>%
		setnames("Height_Stricta",				"S_H")  %>%
		setnames("Percent_Cover_Stricta",		"S_PC") %>%
		as.data.frame	
}

#' Process Plot Survey Data
#'
#' @description Steps:
#' \itemize{
#'  \item fix Cluster variable name
#'  \item include "In Demography Study" information
#'  \item add vegetation information
#'  \item add elevation, distance to water information
#'  \item limit to only plots still in study
#'  \item make coordinates numeric
#' 	}
#'
#' @export

processOccupancyPlotInfo <- function(Plot_Info) {
	#--------------------------------------- Misc
	# fix Cluster name
	"Cluster" -> Plot_Info$Sampling[which(Plot_Info$Sampling=="Clusters")]
	Plot_Info$Island %<>% as.factor
	# file with ALL plots
	# Plot_Info_All <- Plot_Info
	#--------------------------------- include "In Demography Study" information
	Plot_Info %<>% merge(ClustersInDemographicStudy, by = "Cluster", all=T)
	# change NA to "no"
	Plot_Info$InDemographicStudy[which(is.na(Plot_Info$InDemographicStudy))] <- "no"
	# ---------------------------------------- ADD VEGETATION INFO ----------- #
	# has two records for 1572
	Plot_Info %<>% merge(Plot.Vegetation, by="Tag_Number", all=T)
	# ---------------------------------------- ADD GIS INFO ------------------ #
	# GIS_data_updated has remote sensing information, including elevation, dist_water
	Plot_Info %<>% merge(GIS_data_updated, by="Tag_Number", all=T)
	# limit to only plots still in study
	Plot_Info %<>% filter(RemovedFromStudy!="Yes")
	# make coordinates numeric
	Plot_Info[,c(
		"Easting", 
		"Northing")] %<>% 
		apply(., 2, as.numeric
	)
	return(Plot_Info)	
}

#' Process Plot Survey Data
#'
#' @description Steps:
#' \itemize{
	#'  \item limit to only plots still in study
	#'  \item format date
	#'  \item Change Yes/No to 1/0
	#'  \item make variables numeric
	#'  \item If pusilla is not present, change all pusilla-dependent data to NA
	#'  \item If stricta is not present, change all stricta-dependent data to NA
#' 	}
#'
#' @export

processOccupancyPlotSurveys <- function(Plot_Surveys) {
	# ---------------------------- LIMIT TO ONLY SURVEYS OF PLOTS STILL IN STUDY
	# Plot_Surveys_All <- Plot_Surveys
	Plot_Surveys %<>% .[(.$Tag_Number %in% Plot_Info$Tag_Number), ]
	# -------------------------------------------------------------- FORMAT DATE
	Plot_Surveys$Date %<>% Format_Date_Function
	# -------------------------------------------------- CHANGE YES, NO to 0, 1 
	Plot_Surveys[,c(
		"Ammophila",
		"CACA_on_Ammo",
		"MEPR_on_Ammo",
		"CHVI_on_Ammo",
		"DACT_on_Ammo",
		"UnknwnMoth_on_Ammo",
		"Old_Moth_Evidence_Ammophila",
		"Pusilla",
		"CACA_on_Pusilla",
		"MEPR_on_Pusilla",
		"CHVI_on_Pusilla",
		"DACT_on_Pusilla",
		"UnknwnMoth_on_Pusilla",
		"Old_Moth_Evidence_Pusilla",
		"Stricta",
		"CACA_on_Stricta",
		"MEPR_on_Stricta",
		"CHVI_on_Stricta",
		"DACT_on_Stricta",
		"UnknwnMoth_on_Stricta",
		"Old_Moth_Evidence_Stricta")] %<>% 
		apply(., 2, Yes_Function
	)
	Plot_Surveys[,c(
		"Ammophila",
		"CACA_on_Ammo",
		"MEPR_on_Ammo",
		"CHVI_on_Ammo",
		"DACT_on_Ammo",
		"UnknwnMoth_on_Ammo",
		"Old_Moth_Evidence_Ammophila",
		"Pusilla",
		"CACA_on_Pusilla",
		"MEPR_on_Pusilla",
		"CHVI_on_Pusilla",
		"DACT_on_Pusilla",
		"UnknwnMoth_on_Pusilla",
		"Old_Moth_Evidence_Pusilla",
		"Stricta",
		"CACA_on_Stricta",
		"MEPR_on_Stricta",
		"CHVI_on_Stricta",
		"DACT_on_Stricta",
		"UnknwnMoth_on_Stricta",
		"Old_Moth_Evidence_Stricta")] %<>%
		apply(., 2, No_Function
	)
	Plot_Surveys[,c(
		"Ammophila",
		"CACA_on_Ammo",
		"MEPR_on_Ammo",
		"CHVI_on_Ammo",
		"DACT_on_Ammo",
		"UnknwnMoth_on_Ammo",
		"Old_Moth_Evidence_Ammophila",
		"Pusilla",
		"CACA_on_Pusilla",
		"MEPR_on_Pusilla",
		"CHVI_on_Pusilla",
		"DACT_on_Pusilla",
		"UnknwnMoth_on_Pusilla",
		"Old_Moth_Evidence_Pusilla",
		"Stricta",
		"CACA_on_Stricta",
		"MEPR_on_Stricta",
		"CHVI_on_Stricta",
		"DACT_on_Stricta",
		"UnknwnMoth_on_Stricta",
		"Old_Moth_Evidence_Stricta")] %<>% 
		apply(., 2, NA_Function
	)
	# ------------------------------------------------------------- MAKE NUMERIC
	Plot_Surveys[,c(
		"Ammophila",
		"CACA_on_Ammo",
		"MEPR_on_Ammo",
		"CHVI_on_Ammo",
		"DACT_on_Ammo",
		"UnknwnMoth_on_Ammo",
		"Old_Moth_Evidence_Ammophila",
		"Pusilla",
		"CACA_on_Pusilla",
		"MEPR_on_Pusilla",
		"CHVI_on_Pusilla",
		"DACT_on_Pusilla",
		"UnknwnMoth_on_Pusilla",
		"Old_Moth_Evidence_Pusilla",
		"Stricta",
		"CACA_on_Stricta",
		"MEPR_on_Stricta",
		"CHVI_on_Stricta",
		"DACT_on_Stricta",
		"UnknwnMoth_on_Stricta",
		"Old_Moth_Evidence_Stricta")] %<>% 
		apply(., 2, as.numeric
	)
	# ------------------ CHANGE CACTUS DEPENDENT DATA TO NA IF CACTI NOT PRESENT
	# If pusilla is not present, change all pusilla-dependent data to NA
	Plot_Surveys_Pusilla_1 <- Plot_Surveys %>% filter(Pusilla==1)
	Plot_Surveys_Pusilla_0 <- Plot_Surveys %>% 
		filter(Pusilla==0 | is.na(Pusilla))
	Plot_Surveys_Pusilla_0[,c(
		"CACA_on_Pusilla",
		"MEPR_on_Pusilla",
		"CHVI_on_Pusilla",
		"DACT_on_Pusilla",
		"UnknwnMoth_on_Pusilla",
		"Old_Moth_Evidence_Pusilla",
		"Old_Moth_Evidence_recent_Pusilla",
		"Percent_Cover_Pusilla",
		"Height_Pusilla")] %<>% 
		apply(2, NA_Function
	)
	Plot_Surveys <- rbind(Plot_Surveys_Pusilla_1, Plot_Surveys_Pusilla_0)
	# If stricta is not present, change all stricta-dependent data to NA
	Plot_Surveys_Stricta_1 <- Plot_Surveys %>% filter(Stricta==1)
	Plot_Surveys_Stricta_0 <- Plot_Surveys %>% 
		filter(Stricta==0 | is.na(Stricta))
	Plot_Surveys_Stricta_0[,c(
		"CACA_on_Stricta",
		"MEPR_on_Stricta",
		"CHVI_on_Stricta",
		"DACT_on_Stricta",
		"UnknwnMoth_on_Stricta",
		"Old_Moth_Evidence_Stricta",
		"Old_Moth_Evidence_recent_Stricta",
		"Percent_Cover_Stricta",
		"Height_Stricta")] %<>% 
		apply(2, NA_Function
	)
	Plot_Surveys <- rbind(Plot_Surveys_Stricta_1, Plot_Surveys_Stricta_0)
	# ------------------------------------------------------------------------ #
	return(Plot_Surveys)
}


#' fill occupancy plot surveys with info from plant surveys
#' @description This data is from when I was specifically doing occupancy plot surveys.
#' Specific rules for filling the plot surveys:
#' \itemize{
#'  \item percent cover: get from plot survey data; if no plot survey data for that tag number and date, fill with NA
#'  \item height: get from plot survey data; if it was not recorded in the plot survey then use max. plant height from the plant survey data
#'  \item insect presence/absence: get from plot survey data; if it was not recorded in the plot survey then use max. plant height from the plant survey data
#'  \item limit to plot surveys after Dec. 2, 2012 (I wasn't surveying individual plants before this)
#'  \item limit to Tag_Numbers that have marked plants
#' }
#' @param Plot_Surveys
#' @param Plant_Surveys
#' @export

mergePlantSurveysPlotSurveys <- function(Plot_Surveys, Plant_Surveys) {
	temp_A = filter(
		Plot_Surveys, 
		Date > "2012-12-01", Tag_Number %in% Plant_Surveys$Tag_Number
	)
	Z = list()
	# for each tag number in the plot surveys data
	for (i in 1:length(unique(temp_A$Tag_Number))) {
		# pull all records for this Tag Number from plot surveys
		L = filter(temp_A, Tag_Number==unique(temp_A$Tag_Number)[i])
		Z[[i]] <- as.data.frame(matrix(NA,length(L$Date),1))
		Z[[i]][, 1] 					<- L$Tag_Number[1]
		Z[[i]][, "Date"] 				<- L$Date
		Z[[i]][, "Pictures"] 			<- L$Pictures
		Z[[i]][, "PictureDirection"] 	<- L$PictureDirection
		Z[[i]][, "Notes"] 				<- L$Notes
		# for each date
		for (j in 1:dim(L)[1]) {
			# fill with occupancy survey data
			Z[[i]][j, "P_plot_survey"] 	<- L$Pusilla[j]
			Z[[i]][j, "P_plant_survey"] <-  "NA"
			Z[[i]][j, "P_Ca"] 			<- L$P_Ca[j]
			Z[[i]][j, "P_Me"] 			<- L$P_Me[j]
			Z[[i]][j, "P_Ch"] 			<- L$P_Ch[j]
			Z[[i]][j, "P_Umoth"] 		<- L$P_Umoth[j]
			Z[[i]][j, "P_Omoth"] 		<- L$P_Omoth[j]
			Z[[i]][j, "P_PC"] 			<- L$P_PC[j]
			Z[[i]][j, "P_H"] 			<- L$P_H[j]
			Z[[i]][j, "S_plot_survey"] 	<- L$Stricta[j]
			Z[[i]][j, "S_plant_survey"] <- "NA"
			Z[[i]][j, "S_Ca"] 			<- L$S_Ca[j]
			Z[[i]][j, "S_Me"] 			<- L$S_Me[j]
			Z[[i]][j, "S_Ch"] 			<- L$S_Ch[j]
			Z[[i]][j, "S_Umoth"] 		<- L$S_Umoth[j]
			Z[[i]][j, "S_Omoth"] 		<- L$S_Omoth[j]
			Z[[i]][j, "S_PC"] 			<- L$S_PC[j]
			Z[[i]][j, "S_H"] 			<- L$S_H[j]
			# pull all plant survey records for this Tag Number and date from plant surveys
			M = filter(Plant_Surveys, 
				Tag_Number==L$Tag_Number[1], 
				Date==unique(L$Date)[j],
				# remove plants marked as dead
				Dead!=1)
			# remove plants marked as missing	
			M = M[which(M$Missing!=1 | is.na(M$Missing)==T), ]
			# if there is plant survey data for this date, update the occupancy survey data
			# if the M dataframe has at least one row of data:
			if (dim(M)[1] > 0) {
				P = filter(M, Species=="pusilla")
				Z[[i]][j, "P_plot_survey"] 	<- L$Pusilla[j]
				Z[[i]][j, "P_plant_survey"] <- ifelse(dim(P)[1] > 0, 1, 0)
				Z[[i]][j, "P_Ca"] 			<- Maximum(c(P$CA_t, L[j,]$P_Ca))
				Z[[i]][j, "P_Me"] 			<- Maximum(c(P$ME_t, L[j,]$P_Me))
				Z[[i]][j, "P_Ch"] 			<- Maximum(c(P$CH_t, L[j,]$P_Ch))
				Z[[i]][j, "P_Umoth"] 		<- Maximum(c(
												P$Unknown_Moth_t, 
												L[j,]$P_Umoth
											))
				Z[[i]][j, "P_Omoth"] 		<- Maximum(c(
												P$Old_Moth_Evidence_t, 
												L[j,]$P_Omoth
											))
				Z[[i]][j, "P_PC"] 			<- L$P_PC[j]
				# Height - if plot survey height is NA, then replace with max height from plant surveys
				Z[[i]][j, "P_H"] 			<- ifelse(
												is.na(L$P_H[j]) & dim(P)[1] > 0,
												Maximum(P$Height_t), 
												L$P_H[j]
											) 
				S = filter(M, Species=="stricta")
				Z[[i]][j, "S_plot_survey"] 	<- L$Stricta[j]
				Z[[i]][j, "S_plant_survey"] <- ifelse(dim(S)[1] > 0, 1, 0)
				Z[[i]][j, "S_Ca"] 			<- Maximum(c(S$CA_t, L[j,]$S_Ca))
				Z[[i]][j, "S_Me"] 			<- Maximum(c(S$ME_t, L[j,]$S_Me))
				Z[[i]][j, "S_Ch"] 			<- Maximum(c(S$CH_t, L[j,]$S_Ch))
				Z[[i]][j, "S_Umoth"] 		<- Maximum(c(
												S$Unknown_Moth_t, 
												L[j,]$S_Umoth
											))
				Z[[i]][j, "S_Omoth"] 		<- Maximum(c(
												S$Old_Moth_Evidence_t, 
												L[j,]$S_Omoth
											))
				Z[[i]][j, "S_PC"] 			<- L$S_PC[j]
				# Height - if plot survey height is NA, then replace with max height from plant surveys
				Z[[i]][j, "S_H"] <- ifelse(
					is.na(L$S_H[j]) & dim(S)[1] > 0, 
					Maximum(S$Height_t), 
					L$S_H[j]
				)
			}
		}	
	}
	A <- do.call(rbind.data.frame, Z)
	names(A)[1] <- "Tag_Number"
	#  FIX DATA FORMAT
	A[,c(column_list,
		"P_plot_survey", 
		"P_PC", 
		"P_H", 
		"S_plot_survey", 
		"S_PC", 
		"S_H")] %<>%
		apply(., 2, as.numeric
	)
	A[,c(column_list,
		"P_plot_survey", 
		"P_PC", 
		"P_H", 
		"S_plot_survey", 
		"S_PC", 
		"S_H")] %<>%
		apply(., 2, NA_Function
	)
	return(A)
}

#' fill demography plot surveys with info from plant surveys
#' @description I only entered a plot as surveyed in this dataset if I had completely marked/surveyed all plants in the plot. I entered data in this dataset near the beginning of the demography study, when I was setting the study up.
#' Specific rules for filling the plot surveys:
#' \itemize{
#'  \item only use demography plot surveys that are not a duplicate of the occupancy plot surveys
#' }
#' @param D_Plot_Surveys
#' @param Plant_Surveys
#' @export

mergePlotSurveysDemoPlotSurveys <- function(D_Plot_Surveys, Plant_Surveys) {
	Z = list()
	# for each tag number in the demography plot survey data
	for (i in 1:length(unique(D_Plot_Surveys$Tag_Number))) {
		# pull all records for this Tag Number from plot surveys
		L = filter(
			D_Plot_Surveys, 
			Tag_Number==unique(D_Plot_Surveys$Tag_Number)[i]
		)
		Z[[i]] 			<- as.data.frame(matrix(NA,length(unique(L$Date)),1))
		Z[[i]][, 1] 	<- L$Tag_Number[1]
		Z[[i]][, "Date"] <- unique(L$Date)
		# for each date
		for (j in 1:length(unique(L$Date))) {
			# pull all plant survey records for this Tag Number and date from plant surveys
			M = filter(
				Plant_Surveys, 
				Tag_Number==L$Tag_Number[1], 
				Date==unique(L$Date)[j],
				# remove plants marked as dead
				Dead!=1
			)
			# remove plants marked as missing	
			M = M[which(M$Missing!=1 | is.na(M$Missing)==T), ]
			# if there is plant survey data for this date, update the occupancy survey data
			# if the M dataframe has at least one row of data:
			if (dim(M)[1] > 0) {
				P = filter(M, Species=="pusilla")
				Z[[i]][j, "P_plant_survey"] <-  ifelse(dim(P)[1] > 0, 1, 0) 
				Z[[i]][j, "P_Ca"] 			<-  Maximum(P$CA_t) 
				Z[[i]][j, "P_Me"] 			<-  Maximum(P$ME_t) 
				Z[[i]][j, "P_Ch"] 			<-  Maximum(P$CH_t) 
				Z[[i]][j, "P_Umoth"] 		<-  Maximum(P$Unknown_Moth_t) 
				Z[[i]][j, "P_Omoth"] 		<-  Maximum(P$Old_Moth_Evidence_t) 
				Z[[i]][j, "P_H"] 			<- Maximum(P$Height_t)
				S = filter(M, Species=="stricta")
				Z[[i]][j, "S_plant_survey"] <- ifelse(dim(S)[1] > 0, 1, 0) 
				Z[[i]][j, "S_Ca"] 			<- Maximum(S$CA_t) 
				Z[[i]][j, "S_Me"] 			<- Maximum(S$ME_t) 
				Z[[i]][j, "S_Ch"] 			<- Maximum(S$CH_t) 
				Z[[i]][j, "S_Umoth"] 		<- Maximum(S$Unknown_Moth_t) 
				Z[[i]][j, "S_Omoth"] 		<- Maximum(S$Old_Moth_Evidence_t) 
				Z[[i]][j, "S_H"] 			<- Maximum(S$Height_t)
			}
		}
	}
	B <- do.call(rbind.fill, Z)
	names(B)[1] <- "Tag_Number"
	#  FIX DATA FORMAT
	B[,c(
		column_list,
		"P_H", 
		"S_H")] %<>%
		apply(., 2, as.numeric
	)
	B[,c(
		column_list,
		"P_H",  
		"S_H")] %<>%
		apply(., 2, NA_Function
	)
	return(B)	
}

#' Create occupancy plot survey data from plant surveys
#' @description This data is from when I was surveying plants only, not also doing plot surveys.
#' Specific rules for filling the plot surveys:
#' \itemize{
#'  \item I can only add absence data when I know that I surveyed all the plants that day; this requires creating a list of possible plants alive for each survey day
#'  \item filter plant surveys by dates that are not in plot surveys and also not in demographic plot surveys (I don't want to replicate plot, date combos)
#' }
#' @param Plant_Surveys
#' @param Plot_Surveys
#' @param D_Plot_Surveys
#' @export

createPlotSurveysfromPlantSurveys <- function(Plant_Surveys, Plot_Surveys, D_Plot_Surveys) {
	# create Tag/Date Combo Field
	Plant_Surveys 	%<>% mutate(Tag_Date=paste(Tag_Number, Date))
	Plot_Surveys 	%<>% mutate(Tag_Date=paste(Tag_Number, Date))
	D_Plot_Surveys 	%<>% mutate(Tag_Date=paste(Tag_Number, Date))
	# keep records of Tag Numbers not surveyed on particular dates
	temp.plant.surveys <- filter(
		Plant_Surveys, 
		!(Tag_Date %in% Plot_Surveys$Tag_Date),
		!(Tag_Date %in% D_Plot_Surveys$Tag_Date)
	)
	# print plants with Tag_Number==NA as warning
	TagNumbNA <- temp.plant.surveys %>% 
		filter(is.na(temp.plant.surveys$Tag_Number)) %$%
		PlantID
	if (length(TagNumbNA) > 0) {
		warning("Plants with Tag_Number=NA:", paste(TagNumbNA, collapse=","))
	}
	# remove plants with Tag_Number==NA
	temp.plant.surveys <- filter(
		temp.plant.surveys, 
		is.na(temp.plant.surveys$Tag_Number)==FALSE
	)
	Z = list()
	# for each tag number in the demography plot survey data
	for (i in 1:length(unique(temp.plant.surveys$Tag_Number))) {
		# pull all records for this Tag Number from temp.plant.surveys
		L = filter(
			temp.plant.surveys, 
			Tag_Number==unique(temp.plant.surveys$Tag_Number)[i]
		)
		Z[[i]] 			<- as.data.frame(matrix(NA,length(unique(L$Date)),1))	
		Z[[i]][, 1] 	<- L$Tag_Number[1]
		Z[[i]][, "Date"] <- unique(L$Date)
		# for each date
		for (j in 1:length(unique(L$Date))) {
			# pull all plant survey records for this Tag Number and date from plant surveys remove plants marked as missing or dead	
			M = filter(L, Date==unique(L$Date)[j], Dead!=1)
			M = M[which(M$Missing!=1 | is.na(M$Missing)==T), ]
			# get list of PlantIDs for this plot
			N = filter(
				Plant_Info, 
				Tag_Number==L$Tag_Number[1], 
				# only include plants that are listed as having been added to Plant.Info on or after Date
				First.Survey.Date <= unique(L$Date)[j],
				# exclude dead plants (including date plant was first recorded as dead)
				FirstDeadMissingObservation > unique(L$Date)[j] | 
					is.na(FirstDeadMissingObservation)==T
			)
			# if all PlotPlantIDs were surveyed for a given date:
			if (identical(
				M$PlotPlantID[order(M$PlotPlantID)], 
				N$PlotPlantID[order(N$PlotPlantID)]
			)==T) {
				P = filter(M, Species=="pusilla")
				Z[[i]][j, "P_plant_survey"] <- ifelse(dim(P)[1] > 0, 1, 0) 
				Z[[i]][j, "P_Ca"] 			<- Maximum(P$CA_t) 
				Z[[i]][j, "P_Me"] 			<- Maximum(P$ME_t) 
				Z[[i]][j, "P_Ch"] 			<- Maximum(P$CH_t) 
				Z[[i]][j, "P_Umoth"] 		<- Maximum(P$Unknown_Moth_t) 
				Z[[i]][j, "P_Omoth"] 		<- Maximum(P$Old_Moth_Evidence_t) 
				S = filter(M, Species=="stricta")
				Z[[i]][j, "S_plant_survey"] <- ifelse(dim(S)[1] > 0, 1, 0) 
				Z[[i]][j, "S_Ca"] 			<- Maximum(S$CA_t) 
				Z[[i]][j, "S_Me"] 			<- Maximum(S$ME_t) 
				Z[[i]][j, "S_Ch"] 			<- Maximum(S$CH_t) 
				Z[[i]][j, "S_Umoth"] 		<- Maximum(S$Unknown_Moth_t) 
				Z[[i]][j, "S_Omoth"] 		<- Maximum(S$Old_Moth_Evidence_t)
				Z[[i]][j, "all_surveyed"] 	<- "Yes"
		} 
			# if all PlotPlantIDs were NOT surveyed on this date
			else {
				P = filter(M, Species=="pusilla")
				Z[[i]][j, "P_plant_survey"] <- ifelse(dim(P)[1] > 0, 1, NA) 
				Z[[i]][j, "P_Ca"] 			<- mysum1(P$CA_t)
				Z[[i]][j, "P_Me"] 			<- mysum1(P$ME_t)
				Z[[i]][j, "P_Ch"] 			<- mysum1(P$CH_t)
				Z[[i]][j, "P_Umoth"] 		<- mysum1(P$Unknown_Moth_t)
				Z[[i]][j, "P_Omoth"] 		<- mysum1(P$Old_Moth_Evidence_t)
				S = filter(M, Species=="stricta")
				Z[[i]][j, "S_plant_survey"] <- ifelse(dim(S)[1] > 0, 1, NA) 
				Z[[i]][j, "S_Ca"] 			<- mysum1(S$CA_t)
				Z[[i]][j, "S_Me"] 			<- mysum1(S$ME_t)
				Z[[i]][j, "S_Ch"] 			<- mysum1(S$CH_t)
				Z[[i]][j, "S_Umoth"] 		<- mysum1(S$Unknown_Moth_t)
				Z[[i]][j, "S_Omoth"] 		<- mysum1(S$Old_Moth_Evidence_t)
				Z[[i]][j, "all_surveyed"] 	<- "No"
			}
		}
	}	
	C <- do.call(rbind.data.frame, Z)
	names(C)[1] <- "Tag_Number"
	# FIX DATA FORMAT
	C[, column_list] %<>% apply(., 2, as.numeric)
	C[, column_list] %<>% apply(., 2, NA_Function)
	return(C)	
}

#' Create occupancy plot survey data from original plant survey data
#' @description This data is from when I was surveying plants only, not also doing occupancy plot surveys. After IDing and surveying these plants, I deleted these IDs and renamed the plants so the IDs here DO NOT match the surveys and IDs in the demography study data.
#' Specific rules for filling the plot surveys:
#' \itemize{
#'  \item for these dates I am not certain that I surveyed all plants in the plots therefore either something is present or an NA
#' }
#' @param Original_Plant_Data
#' @export

createPlotSurveysfromOrigPlantSurveys <- function(Original_Plant_Data) {
	Z = list()
	# for each tag number in the demography plot survey data
	for (i in 1:length(unique(Original_Plant_Data$Tag_Number))) {
		# pull all records for this Tag Number from temp.plant.surveys
		L = filter(
			Original_Plant_Data, 
			Tag_Number==unique(Original_Plant_Data$Tag_Number)[i]
		)
		Z[[i]] 			<- as.data.frame(matrix(NA,length(unique(L$Date)),1))	
		Z[[i]][, 1] 	<- L$Tag_Number[1]
		Z[[i]][, "Date"] <- unique(L$Date)
		# for each date
		for (j in 1:length(unique(L$Date))) {
			# pull all plant survey records for this Tag Number and date from plant surveys
			M = filter(L, Date==unique(L$Date)[j])
			P = filter(M, HostSpecies=="pusilla")
			Z[[i]][j, "P_plant_survey"] 	<-  ifelse(dim(P)[1] > 0, 1, NA) 
			Z[[i]][j, "P_Ca"] 				<- mysum1(P$CACA_t)
			Z[[i]][j, "P_Me"] 				<- mysum1(P$MEPR_t)
			Z[[i]][j, "P_Ch"] 				<- mysum1(P$CHVI_t)
			Z[[i]][j, "P_Umoth"] 			<- mysum1(P$Unknown_Moth_t)
			Z[[i]][j, "P_Omoth"] 	<- mysum1(P$Old_Moth_Evidence_t)			
			S = filter(M, HostSpecies=="stricta")
			Z[[i]][j, "S_plant_survey"] 	<-  ifelse(dim(S)[1] > 0, 1, NA) 
			Z[[i]][j, "S_Ca"] 				<- mysum1(S$CACA_t)
			Z[[i]][j, "S_Me"] 				<- mysum1(S$MEPR_t)
			Z[[i]][j, "S_Ch"] 				<- mysum1(S$CHVI_t)
			Z[[i]][j, "S_Umoth"]	 		<- mysum1(S$Unknown_Moth_t)
			Z[[i]][j, "S_Omoth"] 			<- mysum1(S$Old_Moth_Evidence_t)
		}
	}	
	D <- do.call(rbind.data.frame, Z)
	names(D)[1] <- "Tag_Number"
	#  FIX DATA FORMAT
	D[, column_list] %<>% apply(., 2, as.numeric)
	D[, column_list] %<>% apply(., 2, NA_Function)
	return(D)	
}

#' Merge Duplicate Surveys
#' @description Merge duplicate surveys (two or more surveys on the same date) and add columns for CACA presence and MEPR presence (regardless of which cactus the insects were found).
#' @param E
#' @export

mergeDuplicateSurveys <- function(E) {
	Z = list()
	# for each tag number
	for (i in 1:length(unique(E$Tag_Number))) {
		# pull all records for this Tag Number from temp.plant.surveys
		L = filter(E, Tag_Number==unique(E$Tag_Number)[i])
		Z[[i]] 			<- as.data.frame(matrix(NA,length(unique(L$Date)),1))
		Z[[i]][, 1] 	<- L$Tag_Number[1]
		Z[[i]][, "Date"] <- unique(L$Date)
		# for each date
		for (j in 1:length(unique(L$Date))) {
			# pull all plant survey records for this Tag Number and date from plant surveys
			M = filter(L, Date==unique(L$Date)[j])
			# pusilla		
			Z[[i]][j, "P_plot_survey"] 		<- max(M$P_plot_survey, na.rm=T)
			Z[[i]][j, "P_plant_survey"] 	<- max(M$P_plant_survey, na.rm=T) 
			Z[[i]][j, "P_Ca"] 				<- max(M$P_Ca, na.rm=T)
			Z[[i]][j, "P_Me"] 				<- max(M$P_Me, na.rm=T)
			Z[[i]][j, "P_Ch"] 				<- max(M$P_Ch, na.rm=T)
			Z[[i]][j, "P_Umoth"] 			<- max(M$P_Umoth, na.rm=T)
			Z[[i]][j, "P_Omoth"] 			<- max(M$P_Omoth, na.rm=T)
			Z[[i]][j, "P_PC"] 				<- max(M$P_PC, na.rm=T)
			Z[[i]][j, "P_H"] 				<- max(M$P_H, na.rm=T)
			# stricta		
			Z[[i]][j, "S_plot_survey"] 		<- max(M$S_plot_survey, na.rm=T)
			Z[[i]][j, "S_plant_survey"] 	<- max(M$S_plant_survey, na.rm=T)
			Z[[i]][j, "S_Ca"] 				<- max(M$S_Ca, na.rm=T)
			Z[[i]][j, "S_Me"] 				<- max(M$S_Me, na.rm=T)
			Z[[i]][j, "S_Ch"] 				<- max(M$S_Ch, na.rm=T)
			Z[[i]][j, "S_Umoth"] 			<- max(M$S_Umoth, na.rm=T)
			Z[[i]][j, "S_Omoth"] 			<- max(M$S_Omoth, na.rm=T)
			Z[[i]][j, "S_PC"] 				<- max(M$S_PC, na.rm=T)
			Z[[i]][j, "S_H"] 				<- max(M$S_H, na.rm=T)
		}
	}
	return(Z)
}

