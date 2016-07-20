#' Merge Plants from Multiple Plots
#' 
#' @param Plant_Surveys Plant survey dataset
#' @description Only merge data collected on the same day. Merge survey data for individual plants in more than one plot. Then combine back into one file with surveys of plants in only one plot.
#' 
#' @export

mergePlantRecordsfromMultiplePlots <- function(Plant_Surveys) {
	# restrict to plants that are in more than one plot
	temp_A <- filter(Plant_Surveys, N.PlotPlantIDs > 1)
	Z = list()
	# for each tag number in the plot surveys data
	for (i in 1:length(unique(temp_A$PlantID))) {
		# pull all records for this Tag Number from plot surveys
		L = filter(temp_A, PlantID==unique(temp_A$PlantID)[i])
		Z[[i]] 	<- as.data.frame(matrix(NA,length(unique(L$Date)),1))	
		Z[[i]][, 1] 					<- L$PlantID[1]
		Z[[i]][, "Date"] 				<- unique(L$Date)
		# can't include unique Tag_Number because some plants are in more than one plot
		Z[[i]][, "ClusterID"] 			<- L$ClusterID[1]
		Z[[i]][, "Network"] 			<- L$Network[1]
		Z[[i]][, "Island"] 				<- L$Island[1]
		Z[[i]][, "Species"] 			<- L$Species[1]
		Z[[i]][, "Easting"] 			<- L$Easting[1]
		Z[[i]][, "Northing"] 			<- L$Northing[1]
		Z[[i]][, "ReproductiveMode"]	<- L$ReproductiveMode %>%
												.[which(. != "NA")] %>%
												.[which(!is.na(.))] %>%
												unique(.) %>%
												paste(collapse="")
		# for each date
		for (j in 1:length(unique(L$Date))) {
			# pull all plant survey records for this date from plant surveys
			M = filter(L, Date==unique(L$Date)[j])
			# get list of PlotPlantIDs
			N = filter(
				Plant_Info, 
				PlantID==L$PlantID[1], 
				# only include plants that are listed as having been added to Plant_Info on or after Date
				First.Survey.Date <= unique(L$Date)[j],
				# exclude dead plants (including date plant was first recorded as dead)
				FirstDeadMissingObservation > unique(L$Date)[j] | 
					is.na(FirstDeadMissingObservation)==T
			)
			# if all PlotPlantIDs were surveyed for a given date:
			if (identical(M$PlotPlantID[order(M$PlotPlantID)], 
				N$PlotPlantID[order(N$PlotPlantID)])==T) {
				Z[[i]][j, "CA_t"] 					<- mysum2(M$CA_t)
				Z[[i]][j, "ME_t"] 					<- mysum2(M$ME_t)
				Z[[i]][j, "CH_t"] 					<- mysum2(M$CH_t)
				Z[[i]][j, "DA_t"] 					<- mysum2(M$DA_t)
				Z[[i]][j, "Unknown_Moth_t"] 		<- mysum2(M$Unknown_Moth_t)
				Z[[i]][j, "Old_Moth_Evidence_t"]<- mysum2(M$Old_Moth_Evidence_t)
				# Dead or missing - has to be dead or missing in all plots
				# (1) if the sum of Dead = # of PlotPlantIDs, the plant is dead in all plots
				if (sum(M$Dead, na.rm=T)==dim(N)[1]) 
					{Z[[i]][j, "Dead"] <- 1} 
					else {Z[[i]][j, "Dead"] <- 0}
				# Missing
				if (sum(M$Missing, na.rm=T)==dim(N)[1]) 
					{Z[[i]][j, "Missing"] <- 1} 
					else {Z[[i]][j, "Missing"] <- 0}
				# all surveyed = TRUE
				Z[[i]][j, "AllSurveyed"] 			<- "TRUE"
			}
			else {
				# if all PlotPlantIDs were NOT surveyed on this date consider the insect to be detected if the sum is greater than zero
				Z[[i]][j, "CA_t"] 					<- mysum1(M$CA_t)
				Z[[i]][j, "ME_t"] 					<- mysum1(M$ME_t)
				Z[[i]][j, "CH_t"] 					<- mysum1(M$CH_t)
				Z[[i]][j, "DA_t"] 					<- mysum1(M$DA_t)
				Z[[i]][j, "Unknown_Moth_t"] 		<- mysum1(M$Unknown_Moth_t)
				Z[[i]][j, "Old_Moth_Evidence_t"]<- mysum1(M$Old_Moth_Evidence_t)
				# Dead or missing - cannot be dead; can be alive if at least one observation of alive
				# (1) if the sum of Dead = # of PlotPlantIDs, the plant is dead in all plots
				if (sum(M$Dead, na.rm=T) <= (dim(N)[1]-1)) 
					{Z[[i]][j, "Dead"] <- 0} 
					else {Z[[i]][j, "Dead"] <- NA}
				# Missing
				Z[[i]][j, "Missing"] <- NA
				# all surveyed = FALSE
				Z[[i]][j, "AllSurveyed"] 			<- "FALSE"
			}
			# Number of segments
			Z[[i]][j, "Size_t"] 					<- mysum(M$Size_t)
			Z[[i]][j, "Plant_Segments_w_leaves"] <- 
				mysum(M$Plant_Segments_w_leaves)
			Z[[i]][j, "Plant_Segments_wo_leaves"] <-
			 	mysum(M$Plant_Segments_wo_leaves)
			Z[[i]][j, "Plant_Segments_woody"] <- mysum(M$Plant_Segments_woody)
			# Size
			Z[[i]][j, "Height_t"] 					<- max(M$Height_t)
			Z[[i]][j, "Width_t"] 					<- max(M$Width_t)
			Z[[i]][j, "Perpen_Width"] 				<- max(M$Perpen_Width)
			# Fruit	and Flowers
			Z[[i]][j, "Num_FlowerBuds"] 			<- mysum(M$Num_FlowerBuds)
			Z[[i]][j, "Num_Fruit_red"] 				<- mysum(M$Num_Fruit_red)
			Z[[i]][j, "Num_Fruit_green"] 			<- mysum(M$Num_Fruit_green)
			Z[[i]][j, "Num_Flowers"] 				<- mysum(M$Num_Flowers)
			Z[[i]][j, "Fruit_t"] 					<- mysum(M$Fruit_t)
			Z[[i]][j, "Fruit_Flowers_t"] 			<- mysum(M$Fruit_Flowers_t)
			Z[[i]][j, "DemographicSurvey"] 			<- M$DemographicSurvey[1]
			Z[[i]][j, "SamplingYear"] 				<- M$SamplingYear[1]
			# Paste PlotPlantIDs together to know which plants were surveyed on this date
			Z[[i]][j, "PlantsSurveyed"] <- paste(M$PlotPlantID, collapse=",")
		}
	}
	# Field indicating all plants that were merged
	# Field indicating the fraction of all plants surveying during this time period
	# Field indicating the dates used for the merge
	# n times surveyed
	# n times insect was found
	# fraction of time that the insect was found
	# field indicating when plant was measured
	temp_B <- do.call(rbind.data.frame, Z)
	names(temp_B)[1] <- "PlantID"
	# max(NA, NA, na.rm=T) returns "-Inf"
	temp_B[,c(
		"Perpen_Width",
		"Width_t",
		"Height_t")] %<>% 
		apply(., 2, NA_Function
	)
	# - Merge plant survey data into one file -------------------------------- #
	# create new file with those plants in only one plot
	temp_C <- filter(Plant_Surveys, N.PlotPlantIDs == 1)
	temp_C %<>% select( 
		PlantID, 
		Date, 
		ClusterID, 
		Network, 
		Island,
		Easting,
		Northing, 
		Species, 
		ReproductiveMode,
		# insects
		CA_t, 
		ME_t, 
		CH_t,
		DA_t,
		Unknown_Moth_t, 
		Old_Moth_Evidence_t, 
		# size
		Size_t, 
		Plant_Segments_w_leaves, 
		Plant_Segments_wo_leaves, 
		Plant_Segments_woody, 
		Height_t, 
		Width_t, 
		Perpen_Width, 
		# fruit
		Num_FlowerBuds, 
		Num_Fruit_red, 
		Num_Fruit_green, 
		Num_Flowers,
		Fruit_t, 
		Fruit_Flowers_t,
		Dead, 
		Missing, 
		DemographicSurvey,
		SamplingYear
	)
	temp_C$AllSurveyed <- "TRUE"
	temp_C$PlantsSurveyed <- "NA"
	# merge plants in multiple plots and plants in one plot
	Plant_Surveys <- rbind.fill(temp_B, temp_C)
	Plant_Surveys %<>% arrange(PlantID, Date)
	return(Plant_Surveys)
}