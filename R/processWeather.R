#' Process Weather Data
#' 
#' @description Steps:
#' \itemize{
#'  \item process weather station (\code{wstations}) list
#'  \item for individual weather station files:
#' 		\itemize{
#' 			\item for each variable, NOAA uses generic column names: "Measurement.Flag", "Quality.Flag", "Source.Flag", "Time.of.Observation"
#' 			\item to ensure that the appropriate columns are merged together, I will rename these columns by pasting the name with the name of the variable to which it refers
#' 			\item e.g., the "Measurement.Flag" column directly after "PRCP" will become "PRCP.Measurement.Flag"
#' 		}
#'  \item Filter data by quality
#' 		\itemize{
#' 			\item replace data with NA if it is of questionable quality (see \code{Quality_Flag_Function} function for details)
#' 			\item replace blank values (quality is okay) with "Okay"
#' 			\item Replace -999 and blanks with NAs
#' 		}
#'  \item Format/convert weather data
#' 		\itemize{
#' 			\item convert tenths of Celcius to Celsius
#' 			\item convert tenths of Celcius to Celsius
#' 			\item convert PRCP (in tenths of mm) to cm
#' 			\item replace NA for precip less than 0
#' 		}
#'  \item Restrict start and end dates
#'  \item Find closest weather stations to each sampling location
#'  \item Choose closest weather variable measurement for each Location/Date combo
#'  \item Format Time variables
#'  \item calculate growing degree days
#' 		\itemize{
#' 			\item used this website to calculate growing degree days: http://www.ipm.ucdavis.edu/WEATHER/ddretrievetext.html
#' 		}
#'  \item save data
#' }
#' @param timeseries
#' @export

processWeather <- function() {
	wstations <- ghcnd.stations.NW.FL.current
	Location_list <- c("BLSP", "HBSP", "MB", "N", "SASP", "TSP")
	#--------------------------------------------------------------------------#
	# Weather Station Info
	#--------------------------------------------------------------------------#
	# columns for Start and End Dates of Weather Station
	wstations$Start_Date <- sub(" .*", "", 
		wstations$Date_Range)
	wstations$End_Date <- sub(".* ", "", 
		wstations$Date_Range)
	# filter dates to study dates
	wstations %<>%
		filter(Start_Date <= "2014-01-17", End_Date >= "2008-01-20") %>%
		# merge with list of weather stations for which start/end date is not known
		rbind(filter(wstations, Start_Date == "", End_Date >= ""))
	############################################################################
	# MODIFY INDIVIDUAL WEATHER STATION FILES
	############################################################################
	#--------------------------------------------------------------------------#
	# Merge Files Into One
	#--------------------------------------------------------------------------#
	# for each variable, NOAA uses generic column names "Measurement.Flag", 
	#		"Quality.Flag", "Source.Flag", "Time.of.Observation"
	# to ensure that the appropriate columns are merged together, I will rename 
	#		these columns by pasting the name with the name of the variable to 
	#		which it refers
	# e.g., the "Measurement.Flag" column directly after "PRCP" will become 
	#		"PRCP.Measurement.Flag"
	# list of climate files to fix
	climate_file_names <- c("NOAA.ApalachicolaAirport", "NOAA.ApalachicolaAirport1", "NOAA.Bloxham", "NOAA.Bristol2", "NOAA.Callaway03", "NOAA.Callaway06S", "NOAA.Chipley", "NOAA.Clarksville2N", "NOAA.CrestviewBobSikesAirport", "NOAA.DeFuniakSprings", "NOAA.Destin1", "NOAA.DestinFortWaltonBeach", "NOAA.Freeport34", "NOAA.Freeport40SSW", "NOAA.Freeport44", "NOAA.Hosford27", "NOAA.Marianna7NE", "NOAA.NavalLiveOaks", "NOAA.NWFLBeaches", "NOAA.PanamaCity", "NOAA.PanamaCityBayCoAirport", "NOAA.PanamaCityBeach03", "NOAA.PanamaCityBeach12ESE", "NOAA.PensacolaForest", "NOAA.PensacolaRegionalAirport", "NOAA.PortStJoe06", "NOAA.PortStJoe81", "NOAA.Quincy3SSW", "NOAA.Sumatra", "NOAA.Telogia", "NOAA.ValparaisoEglinAFB", "NOAA.Vernon106", "NOAA.WestPensacola109", "NOAA.Wewahitchka", "NOAA.Wewahitchka16", "NOAA.Wilma", "NOAA.WoodruffDam", "NOAA.Bellview17", "NOAA.Pensacola92", "NOAA.MiramarBeach95", "NOAA.InletBeach07", "NOAA.PanamaCityBeach59", "NOAA.Apalachicola08WNW", "NOAA.NewHope", "NOAA.Destin15")
	# for each weather variable, take weather variable name and paste it to the 
	#		names of the 4 following columns 
	X <- list()
	for (h in 1:length(climate_file_names)) {
		# PRCP, TMIN, TMAX name vectors
		TMIN_vector <- vector()
		TMAX_vector <- vector()
		PRCP_vector <- vector()
		# pull climate file
		Y <- eval(parse(text=climate_file_names[h]))
		X[[h]] <- list()
		# Weather station info
		X[[h]][[1]] <- Y %>% select(STATION, STATION_NAME, ELEVATION, LATITUDE, 
			LONGITUDE, DATE)
		# counter for weather variable (have to count because not all files have all 
			# weather variables)
		k=2 # start at 2 because weather station info goes in [[1]]
		#######
		# PRCP
		#######
		if ("PRCP" %in% names(Y)) {
			X[[h]][[k]] <- Y[, (which(colnames(Y)=="PRCP")) : 
				(which(colnames(Y)=="PRCP")+4)]
			# fix PRCP column names
			for (i in 1:4) {
				PRCP_vector[i] <-
				paste("PRCP", sub("(.*?)[.].*", "\\1", names(X[[h]][[k]][which(colnames(X[[h]][[k]])=="PRCP") + i])), "Flag", sep=".")
			}
			names(X[[h]][[k]])[2:5] <- PRCP_vector
		k=k+1
		}
		# fix TMIN columns
		if ("TMIN" %in% names(Y)) {
			X[[h]][[k]] <- Y[, (which(colnames(Y)=="TMIN")) : 
				(which(colnames(Y)=="TMIN")+4)]
			for (i in 1:4) {
				TMIN_vector[i] <-
				paste("TMIN", sub("(.*?)[.].*", "\\1", names(X[[h]][[k]][which(colnames(X[[h]][[k]])=="TMIN") + i])), "Flag", sep=".")
			}
			names(X[[h]][[k]])[2:5] <- TMIN_vector
			k=k+1
		}
		# fix TMAX columns
		if ("TMAX" %in% names(Y)) {
			X[[h]][[k]] <- Y[, (which(colnames(Y)=="TMAX")) : 
				(which(colnames(Y)=="TMAX")+4)]
			for (i in 1:4) {
				TMAX_vector[i] <-
				paste("TMAX", sub("(.*?)[.].*", "\\1", names(X[[h]][[k]][which(colnames(X[[h]][[k]])=="TMAX") + i])), "Flag", sep=".")		
			}
			names(X[[h]][[k]])[2:5] <- TMAX_vector
		}
	}
	# merge list of lists of lists
	data.array2 <- list()
	for (i in 1:length(unique(climate_file_names))) {
		data.array2[[i]] = as.data.frame(
			mapply(cbind, unlist(X[[i]], recursive=F))
		)
	}	
	climate_data = do.call(rbind.fill, data.array2)
	#--------------------------------------------------------------------------#
	# Filter data by quality
	#--------------------------------------------------------------------------#
	# replace data with NA if it is of questionable quality
	climate_data$PRCP <- with(climate_data, Quality_Flag_Function(PRCP, 
		PRCP.Quality.Flag))
	climate_data$TMIN <- with(climate_data, Quality_Flag_Function(TMIN, 
		TMIN.Quality.Flag))
	climate_data$TMAX <- with(climate_data, Quality_Flag_Function(TMAX, 
		TMAX.Quality.Flag))
	# replace blank values (quality is okay) with "Okay"
	climate_data[,c(
		"PRCP.Quality.Flag", 
		"TMIN.Quality.Flag",
		"TMAX.Quality.Flag")] %<>% 
		apply(., 2, Replace_Blank_w_Okay_Function
	)
	# Replace -999 and blanks with NAs
	climate_data[,c(
		"STATION", 
		"STATION_NAME",
		"ELEVATION",
		"LATITUDE",
		"LONGITUDE",
		"DATE",
		"PRCP",
		"PRCP.Measurement.Flag",
		"PRCP.Quality.Flag",
		"PRCP.Source.Flag",
		"PRCP.Time.Flag",
		"TMIN",
		"TMIN.Measurement.Flag",
		"TMIN.Quality.Flag",
		"TMIN.Source.Flag",
		"TMIN.Time.Flag",
		"TMAX",
		"TMAX.Measurement.Flag",
		"TMAX.Quality.Flag",
		"TMAX.Source.Flag",
		"TMAX.Time.Flag"
	)] %<>% apply(., 2, NA_Function)
	#------------------------------------- Format/convert weather data -------#
	climate_data[,c(
		"PRCP",
		"TMIN",
		"TMAX"
	)] %<>% apply(., 2, as.numeric)
	climate_data %<>% 
		mutate(
			MinTemp = TMIN/10, # convert tenths of Celcius to Celsius
			MaxTemp = TMAX/10, # convert tenths of Celcius to Celsius
			Precip = PRCP/100, # convert PRCP (in tenths of mm) to cm
			Date = as.Date(as.character(DATE), "%Y%m%d")
		)
	# replace NAs again just in case
	climate_data[,c("MinTemp","MaxTemp","Precip")] %<>% apply(., 2, NA_Function)
	# replace NA for precip
	climate_data$Precip[which(climate_data$Precip < 0)] <- NA
	#---------------------------- Restrict start and end dates --------------- #
	climate_data %<>% filter(Date <= "2014-01-17", Date >= "2008-01-20")
	#-------------- Find closest weather stations to each sampling location -- #
	# DISTANCE MATRIX
	# merge sampling locations and weather station locations to calculate distance
	#		matrix (all pairwise distances among points)
	A <- Marsico.Florida.data.collection.site.location.summary %>%
		dplyr::select(Location.name, Latitude, Longitude) %>%
		rbind.fill(dplyr::select(wstations, Name, Latitude, Longitude))
	# first convert sampling locations and weather station coordinates to UTM
	coordinates(A) <- c("Longitude", "Latitude")
	proj4string(A) <- CRS("+proj=longlat +datum=WGS84")  ## for example
	# then calculate dist matrix (which will now be in m)
	A %<>% spTransform(CRS("+proj=utm +zone=16 ellps=WGS84"))
	# convert coordinates back to numeric
	A <- as.data.frame(cbind(A$Name, A@coords))
	names(A) <- c("Name", "Easting", "Northing")
	A[,c("Easting", "Northing")] %<>% apply(., 2, as.numeric)
	# convert coordinate units to km
	A[,c("Easting", "Northing")] %<>% apply(., 2, function(x) {x/1000})
	# calculate distance matrix
	distance_matrix <- as.data.frame(as.matrix(dist(cbind(A$Easting, A$Northing), method="euclidian")))
	# modify distance matrix
	# 		keep columns 1:6 (correspond to the 6 sampling locations)
	# 		keep rows 7:170 (correspond to the weather stations)
	distance_matrix <- distance_matrix[7:134, 1:6]
	names(distance_matrix) <- Location_list
	# merge distance matrix with weather station info
	B <- dplyr::select(wstations, Name, Station.ID, Latitude, Longitude, Precipitation, Temperature, Used_in_Analysis, Sampling_Site, Near_Sampling_Site, Date_Range)
	distance_matrix %<>% cbind(B)
	# change distance matrix columns to one column
	weather_station_info <- reshape2::melt(distance_matrix, id.vars=c("Name", "Station.ID", "Latitude", "Longitude", "Precipitation", "Temperature", "Used_in_Analysis", "Sampling_Site", "Near_Sampling_Site", "Date_Range"))
	names(weather_station_info)[(dim(weather_station_info)[2]-1):dim(weather_station_info)[2]] <- c("Sampling_Location", "Distance")
	# standardize Station IDs - remove "GHCND:" if the the ID has it
	weather_station_info$Station.ID <- gsub("^.*\\:", "", weather_station_info$Station.ID)
	climate_data$STATION <- gsub("^.*\\:", "", climate_data$STATION)
	# SELECT CLOSEST WEATHER STATIONS FOR EACH SAMPLING LOCATION
	# merge distance data with climate_data
	climate_data_temp <- weather_station_info %>% 
		dplyr::select(Station.ID, Sampling_Location, Distance) %>%
		merge(climate_data, by.x="Station.ID", by.y="STATION")
	BLSP_stations <- climate_data_temp %>% filter(Sampling_Location=="BLSP" & Distance <= 85)
	HBSP_stations <- climate_data_temp %>% filter(Sampling_Location=="HBSP" & Distance <= 85) %>% arrange(Date)
	MB_stations <- climate_data_temp %>% filter(Sampling_Location=="MB" & Distance <= 85)
	N_stations <- climate_data_temp %>% filter(Sampling_Location=="N" & Distance <= 85)
	SASP_stations <- climate_data_temp %>% filter(Sampling_Location=="SASP" & Distance <= 85)
	TSP_stations <- climate_data_temp %>% filter(Sampling_Location=="TSP" & Distance <= 85)
	#Choose closest weather variable measurement for each Location/Date combo -#
	X <- list()
	# for each LOCATION
	for (i in 1:length(Location_list)) {
		X[[i]] <- list()
		# pull climate data for that location and merge with station data to get 
		#		distance from station to sampling location
		data = eval(parse(text=paste(Location_list[i], "_stations", sep="")))
		# for PRECIPITATION
		P <- data %>% filter(!is.na(Precip))
			X[[i]][[1]] <- as.data.frame(matrix(NA, length(unique(P$Date)), 6))
			names(X[[i]][[1]]) <- c("Precip_STATION_NAME", "Precip_STATION", 
				"Precip_STATION_Distance", "Date", "Precip", "Location")
			# for each DATE
			for (j in 1:length(unique(P$Date))) {
				# pull climate data associated with that location
				X[[i]][[1]][j, ] = P %>% filter(Date==unique(Date)[j]) %>% 
					filter(Distance==min(Distance)) %>%
					summarise(
					Precip_STATION_NAME = STATION_NAME,
					Precip_STATION = Station.ID,
					Precip_STATION_Distance = Distance,
					Date = Date[1],
					Precip = Precip
				)
			}
		# for MIN TEMPERATURE
		P <- data %>% filter(!is.na(MinTemp))
			X[[i]][[2]] <- as.data.frame(matrix(NA, length(unique(P$Date)), 6))
			names(X[[i]][[2]]) <- c("MinTemp_STATION_NAME", "MinTemp_STATION", 
				"MinTemp_STATION_Distance", "Date", "MinTemp", "Location")
			# for each DATE
			for (j in 1:length(unique(P$Date))) {
				# pull climate data associated with that location
				X[[i]][[2]][j, ] = P %>% filter(Date==unique(Date)[j]) %>% 
					filter(Distance==min(Distance)) %>%
					summarise(
					MinTemp_STATION_NAME = STATION_NAME,
					MinTemp_STATION = Station.ID,
					MinTemp_STATION_Distance = Distance,
					Date = Date,
					MinTemp = MinTemp
				)
			}
		# for MAX TEMPERATURE
		P <- data %>% filter(!is.na(MaxTemp))
			X[[i]][[3]] <- as.data.frame(matrix(NA, length(unique(P$Date)), 6))
			names(X[[i]][[3]]) <- c("MaxTemp_STATION_NAME", "MaxTemp_STATION", 
				"MaxTemp_STATION_Distance", "Date", "MaxTemp", "Location")
			# for each DATE
			for (j in 1:length(unique(P$Date))) {
				# pull climate data associated with that location
				X[[i]][[3]][j, ] = P %>% filter(Date==unique(Date)[j]) %>% 
					filter(Distance==min(Distance)) %>%
					summarise(
					MaxTemp_STATION_NAME = STATION_NAME,
					MaxTemp_STATION = Station.ID,
					MaxTemp_STATION_Distance = Distance,
					Date = Date,
					MaxTemp = MaxTemp
				)
			}
			X[[i]][[1]]$Date %<>% as.Date
			X[[i]][[2]]$Date %<>% as.Date
			X[[i]][[3]]$Date %<>% as.Date
			X[[i]][[1]]$Location = Location_list[i]
			X[[i]][[2]]$Location = Location_list[i]
			X[[i]][[3]]$Location = Location_list[i]
	}
	# merge list of lists of lists
	data.array2 <- list()
	# for each density
	for (i in 1:length(Location_list)) {
	# for (i in 1:length(unique(patch_data$density))) {
		# compress list of statistics to dataframe
		data.array2[[i]] = join_all(X[[i]], by="Date", type="full")
	}	
	climate_data = do.call(rbind.fill, data.array2)
	############################################################################
	# Format Time variables
	############################################################################
	climate_data %<>% 
		mutate(
			Year = year(Date),
			Day_of_year = as.numeric(strftime(Date, format = "%j")),
			Precip_Presence = ifelse(Precip>0, 1, 0),
			MinTemp_lt_equal_0 = ifelse(MinTemp<=0, 1, 0)
		)			
	############################################################################
	# calculate growing degree days
	############################################################################
	# used this website to calculate growing degree days: http://www.ipm.ucdavis.edu/WEATHER/ddretrievetext.html
	# merge UCD IPM files into one
	DegreeDays = rbind.fill(
	UCD.IPM.BLSPDegree.days,
	UCD.IPM.HBSPDegree.days,
	UCD.IPM.MexicoBeachDegree.days,
	UCD.IPM.NokuseDegree.days,
	UCD.IPM.SASPDegree.days,
	UCD.IPM.SweetwaterDegree.days)
	DegreeDays$Date %<>% as.Date("%m/%d/%y")
	DegreeDays.unique = unique(DegreeDays[, 1:4])
	DegreeDays %>% filter(Date=="2014-01-17")
	DegreeDays.merged = merge(
		DegreeDays.unique, 
		climate_data, 
		by.x=c("Date", "Temp.min", "Temp.max"), 
		by.y=c("Date", "MinTemp", "MaxTemp"), 
		all.y=T
	)
	names(DegreeDays.merged)[2:3] <- c("MinTemp", "MaxTemp")
	DegreeDays.merged -> climate_data
	############################################################################
	# PROCESS MERGED CLIMATE DATA
	############################################################################
	# if MinTemp > MaxTemp, replace both with NA
	temp = as.vector(which(climate_data$MinTemp > climate_data$MaxTemp))
	climate_data[temp, ]$MinTemp <- NA
	climate_data[temp, ]$MaxTemp <- NA
	# if MinTemp == MaxTemp, replace both with NA
	temp = as.vector(which(climate_data$MinTemp == climate_data$MaxTemp))
	climate_data[temp, ]$MinTemp <- NA
	climate_data[temp, ]$MaxTemp <- NA
	############################################################################
	# Misc
	############################################################################
	# rename Mexico Beach
	# climate_data$Location[climate_data$Location == 'MexicoBeach'] <- 'Mexico Beach'
	############################################################################
	# SAVE
	############################################################################
	setwd("/Users/KSauby/Documents/Dropbox/GradSchool/Research/Projects/marsico-time-series/")
	cache("climate_data")
	write.csv(climate_data, "./data/climate_data_processed.csv")
	return(climate_data)
}


#' Filter Data by Quality Flag
#' @description If the quality flag from a NOAA dataset is G, I, K, L, N, O, then change the weather value to "NA"
#' Table 2 (Quality Flag/Attribute) from the NOAA documentation:
#' \itemize{
#'  \item Blank = did not fail any quality assurance check D = failed duplicate check
#'  \item G = failed gap check
#'  \item I = failed internal consistency check
#'  \item K = failed streak/frequent-value check
#'  \item L = failed check on length of multiday period M = failed mega-consistency check
#'  \item N = failed naught check
#'  \item O = failed climatological outlier check
#' 	}
#' @param x
#' @param y

Quality_Flag_Function <- function(x, y){	
	x[which(y=="G" | y=="I" | y=="K" | y=="L" | y=="N" | y=="O")] <- NA
	return(x)
}

#' Replace blank values (quality is okay) with "Okay"
#' @param x

Replace_Blank_w_Okay_Function <- function(x){	
	x[which(x==" ")] <- "Okay"
	return(x)
}






