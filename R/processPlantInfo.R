#' Process Plant Info
#'
#' @description Process Plant Info
#' @param Plant_Info Dataset with Plant Information
#'
#' @importFrom dplyr select summarise group_by arrange
#' @export

processPlantInfo <- function(Plant_Info, Plot_Info) {
	# ----------------- ADD INFO FROM Plot_Info (Cluster, Network, Island, etc.)
	Plant_Info <- Plot_Info %>%
		dplyr::select(
			Island, 
			Tag_Number, 
			Cluster, 
			Cluster2, 
			Network, 
			Sampling, 
			SurveyOrder
		) %>%
		merge(Plant_Info, by = "Tag_Number", all.y=TRUE) %>%
		as.data.table %>%
		setnames("HostSpecies", "Species") %>%
		as.data.frame
	#---------------------------------------------------------- FORMAT PLANT IDs
	Plant_Info %<>% Format_PlantIDs_Function
	#---------------------- CALCULATE AND ADD NUMBER OF PlotPlantIDs PER PlantID
	Plant_Info <- Plant_Info %>%
		group_by(PlantID) %>%
		summarise(
			N.PlotPlantIDs = length(unique(PlotPlantID))
		) %>%
		merge(Plant_Info, by="PlantID")
	#----------------------------------- ADD FIRST DATE PlotPlantID WAS SURVEYED
	# particularly relevant for plants that grew into plots over the course of the study (and thus the number of PlotPlantIDs for a given PlantID changed over time)
	Plant_Info <- Plant_Surveys %>%
		group_by(PlotPlantID) %>%
		summarise(
			First.Survey.Date = min(Date)
		) %>%
		merge(Plant_Info, ., by="PlotPlantID", all.y=TRUE)
	# ----------------------------------------------------------- PLANT SURVIVAL
	# indicate whether plant was previously listed as dead or missing
	# group based on plant ID number
	Plant_Info <- Plant_Surveys %>% group_by(PlotPlantID) %>%
		summarise( 
		# a plant has been obvserved to be dead/missing if at least 1 observation (> 0)
		DeadObservation 		= Dead_Missing_Function(Dead),
		MissingObservation 		= Dead_Missing_Function(Missing),
		# a plant is confirmed dead/missing if at least 2 observations (> 1)
		ConfirmedDead 			= Confirmed_Dead_Missing_Function(Dead),
		ConfirmedMissing 		= Confirmed_Dead_Missing_Function(Missing),
		ConfirmedDeadMissing = Confirmed_Dead_Missing_Function(c(Dead,Missing))	
	) %>%
	merge(Plant_Info, ., by="PlotPlantID")
	#---------------- ADD FIRST DATE PlotPlantID WAS RECORDED AS DEAD OR MISSING
	# oldest date PlotPlantID was recorded as dead
	temp_dead_obs <- filter(Plant_Surveys, Dead=="1") %>%
		group_by(PlotPlantID) %>%
		summarise(FirstDeadObservation = min(Date))
	# oldest date PlotPlantID was recorded as missing
	temp_missing_obs <- filter(Plant_Surveys, Missing=="1") %>%
		group_by(PlotPlantID) %>%
		summarise(FirstMissingObservation = min(Date))
	# oldest date PlotPlantID was recorded as missing or dead
	temp_dead_missing <- merge(
		temp_dead_obs, 
		temp_missing_obs, 
		by="PlotPlantID", 
		all=T
	) 
	temp_dead_missing$FirstDeadMissingObservation = 
		dplyr::select(
			temp_dead_missing, 
			FirstDeadObservation,
			FirstMissingObservation
		) %>% 
		apply(., 1, min, na.rm=T) 
	# merge with Plant_Info
	Plant_Info <- merge(Plant_Info, temp_dead_missing, by="PlotPlantID", all=T)
	# -------------------------------------------------- CLEANUP FOR CONSISTENCY
	Plant_Info[,c(
		"Quadrant",
		"ReproductiveMode",
		"Parent")] %<>%
		apply(., 2, as.character
	)
	Plant_Info[,c(
		"Quadrant",
		"ReproductiveMode",
		"Parent")] %<>%
		apply(., 2, NA_Function
	)
	Plant_Info$ReproductiveMode %<>% WoodyTrunk_Function()
	Plant_Info[,c(
		"Quadrant",
		"ReproductiveMode",
		"Parent")] %<>%
		apply(., 2, as.factor
	)
	# ------------------------------------------------- ADD InDemomographicStudy
	# save all Plant_Info
	Plant_Info_All <- Plant_Info
	Plant_Info %<>% merge(
		., 
		ClustersInDemographicStudy, 
		by = "Cluster",
		all=TRUE
	) 
	# ------------------------------------------------------------ ADD ClusterID
	#	do this because some clusters share plots
	Plot_Info_Cluster <- Plot_Info %>%
		dplyr::select(Tag_Number, Cluster, Cluster2) %>%
		reshape2:::melt.data.frame(., id.vars=c("Tag_Number"), 
			value.name="ClusterID") %>%
		filter(ClusterID!=0) %>%
		.[, -2] %>%
		arrange(Tag_Number)
	# CLUSTER ID FOR PLOTS *NOT* IN CLUSTERS
	temp_A = Plot_Info %>%
		dplyr::select(Tag_Number, Cluster) %>%
		filter(Cluster==0)
	temp_A$ClusterID <- temp_A$Tag_Number
	temp_A %<>% .[, -2]
	Plot_Info_Cluster %<>% rbind.fill(temp_A) %>% 
		merge(Plot_Info, by="Tag_Number") %>%
		dplyr::select(ClusterID, Tag_Number)
	Plant_Info %<>% merge(Plot_Info_Cluster, by="Tag_Number", all.x=T)
	return(Plant_Info)
}
	
#' Save File with Confirmed Dead and Missing Plants
#'
#' @description write csv with dead/missing plants - remove them from the maps
#' @param Plant_Info Dataset with Plant Information
#'
#' @export
	
saveConfirmedDeadMissing <- function(Plant_Info) {
	Plant_Info_Dead <- filter(
		Plant_Info, 
		ConfirmedDead=="1" | ConfirmedMissing=="1"
	)
	Plant_Info_Dead %<>% arrange(Tag_Number, PlantID)
}