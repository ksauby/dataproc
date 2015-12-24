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
#' @export
#' @importFrom dplyr select

Quality_Flag_Function <- function(x, y){
	x[which(y=="G" | y=="I" | y=="K" | y=="L" | y=="N" | y=="O")] <- NA
	return(x)
}

#' Replace blank values (quality is okay) with "Okay"
#' @param x
#' @export

Replace_Blank_w_Okay_Function <- function(x){
	x[which(x==" ")] <- "Okay"
	return(x)
}

#' Format Weather Station Info
#' @param wstations List of weather stations, downloaded from the NOAA NCDC site
#' @export

formatWeatherStationInfo <- function(wstations) {
	# create columns for Start and End Dates of Weather Station
	wstations$Start_Date <- sub(" .*", "", 
		wstations$Date_Range)
	wstations$End_Date <- sub(".* ", "", 
		wstations$Date_Range)
	# filter dates to study dates
	wstations %<>%
		filter(Start_Date <= "2014-01-17", End_Date >= "2008-01-20") %>%
		# merge with list of weather stations for which start/end date is not known
		rbind(filter(wstations, Start_Date == "", End_Date >= ""))
	return(wstations)
}

#' Merge Weather Data Files and Format Column Names
#' @param climate_data
#' @description For each variable, NOAA uses generic column names "Measurement.Flag", "Quality.Flag", "Source.Flag", "Time.of.Observation" to ensure that the appropriate columns are merged together. This function renames these columns by pasting the name with the name of the variable to which it refers e.g., the "Measurement.Flag" column directly after "PRCP" will become "PRCP.Measurement.Flag" for each weather variable, take weather variable name and paste it to the names of the 4 following columns.
#' @export

mergeClimateFiles <- function(climate_file_names) {
	X <- list()
	for (h in 1:length(climate_file_names)) {
		# PRCP, TMIN, TMAX name vectors
		TMIN_vector <- vector()
		TMAX_vector <- vector()
		PRCP_vector <- vector()
		# pull climate file
		Y <- eval(parse(text=climate_file_names[h]))
		X[[h]] <- list()
		# select weather station info to keep
		X[[h]][[1]] <- Y %>% dplyr::select(
			STATION, 
			STATION_NAME, 
			ELEVATION, 
			LATITUDE, 
			LONGITUDE, 
			DATE
		)
		# counter for weather variable (have to count because not all files have all weather variables)
		k=2 # start at 2 because weather station info goes in [[1]]
		# fix PRCP flag column names
		if ("PRCP" %in% names(Y)) {
			X[[h]][[k]] <- Y[, (which(colnames(Y)=="PRCP")) : 
				(which(colnames(Y)=="PRCP")+4)]
			for (i in 1:4) {
				PRCP_vector[i] <- paste(
					"PRCP", 
					sub(
						"(.*?)[.].*", "\\1", 
						names(X[[h]][[k]][which(colnames(X[[h]][[k]])=="PRCP") 
							+ i])
					), 
					"Flag", 
					sep="."
				)
			}
			names(X[[h]][[k]])[2:5] <- PRCP_vector
			k <- k + 1
		}
		# fix TMIN flag column names
		if ("TMIN" %in% names(Y)) {
			X[[h]][[k]] <- Y[, (which(colnames(Y)=="TMIN")) : 
				(which(colnames(Y)=="TMIN")+4)]
			for (i in 1:4) {
				TMIN_vector[i] <- paste(
					"TMIN", 
					sub(
						"(.*?)[.].*", "\\1", 
						names(X[[h]][[k]][which(colnames(X[[h]][[k]])=="TMIN") + i])
					), 
					"Flag", 
					sep="."
				)
			}
			names(X[[h]][[k]])[2:5] <- TMIN_vector
			k <- k + 1
		}
		# fix TMAX flag column names
		if ("TMAX" %in% names(Y)) {
			X[[h]][[k]] <- Y[, (which(colnames(Y)=="TMAX")) : 
				(which(colnames(Y)=="TMAX")+4)]
			for (i in 1:4) {
				TMAX_vector[i] <- paste(
					"TMAX", 
					sub(
						"(.*?)[.].*", "\\1", 
						names(X[[h]][[k]][which(colnames(X[[h]][[k]])=="TMAX") + i])
					), 
					"Flag", 
					sep="."
				)		
			}
			names(X[[h]][[k]])[2:5] <- TMAX_vector
		}
	}
	# merge list of lists of lists
	data.array2 <- list()
	for (i in 1:length(unique(climate_file_names))) {
		data.array2[[i]] = as.data.frame(
			mapply(
				cbind, 
				unlist(
					X[[i]], 
					recursive=F
				)
			)
		)
	}	
	do.call(rbind.fill, data.array2)
}

#' Filter data by quality
#' @param climate_data
#' @description Replace data with NA if it is of questionable quality.
#' \itemize{ 
#' 		\item Filter data by quality
#' 		\itemize{
#' 			\item replace data with NA if it is of questionable quality (see \code{Quality_Flag_Function} function for details)
#' 			\item replace blank values (quality is okay) with "Okay"
#' 			\item Replace -999 and blanks with NAs
#' 		}
#' }
#' @export

filterClimateDataByQuality <- function(climate_data) {
	climate_data$PRCP <- with(
		climate_data, 
		Quality_Flag_Function(PRCP, PRCP.Quality.Flag)
	)
	climate_data$TMIN <- with(
		climate_data, 
		Quality_Flag_Function(TMIN, TMIN.Quality.Flag)
	)
	climate_data$TMAX <- with(
		climate_data, 
		Quality_Flag_Function(TMAX, TMAX.Quality.Flag)
	)
	# replace blank values (quality is okay) with "Okay"
	climate_data[,c(
		"PRCP.Quality.Flag", 
		"TMIN.Quality.Flag",
		"TMAX.Quality.Flag"
	)] %<>% apply(., 2, Replace_Blank_w_Okay_Function)
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
	return(climate_data)
}

#' Format and convert weather data
#' @param climate_data
#' @description For each location, compile weather data from the closest weather stations.
#' \itemize{
#' 	\item Format/convert weather data
#' 	\itemize{
#' 		\item convert tenths of Celcius to Celsius
#' 		\item convert tenths of Celcius to Celsius
#' 		\item convert PRCP (in tenths of mm) to cm
#' 		\item replace NA for precip less than 0
#' 	}
#' }
#' @export

formatconvertClimateData <- function(climate_data) {
	climate_data[,c(
		"PRCP",
		"TMIN",
		"TMAX"
	)] %<>% apply(., 2, as.numeric)
	climate_data %<>% mutate(
			MinTemp = TMIN/10, # convert tenths of Celcius to Celsius
			MaxTemp = TMAX/10, # convert tenths of Celcius to Celsius
			Precip = PRCP/100, # convert PRCP (in tenths of mm) to cm
			Date = as.Date(as.character(DATE), "%Y%m%d")
		)
	# replace NAs again just in case
	climate_data[,c("MinTemp","MaxTemp","Precip")] %<>% apply(., 2, NA_Function)
	# replace NA for precip
	climate_data$Precip[which(climate_data$Precip < 0)] <- NA
	return(climate_data)
}

#' Find closest weather stations to each sampling location
#' @description For each location, compile weather data from the closest weather stations.
#' @param sites List of sampling locations with x, y coordinates.
#' @param climate_data
#' @param Distance Radius (km) within which to look for climate stations for a particular location. Defaults to 85 kilometers.
#' @export

findClosestWeatherStations <- function(sites, climate_data, Distance=85) {
	# merge sampling locations and weather station locations to calculate distance matrix (all pairwise distances among points)
	A <- sites %>%
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
	distance_matrix <- as.data.frame(
		as.matrix(
			dist(
				cbind(A$Easting, A$Northing), 
				method="euclidian"
			)
		)
	)
	# modify distance matrix
	# 		keep columns 1:6 (correspond to the 6 sampling locations)
	# 		keep rows 7:170 (correspond to the weather stations)
	distance_matrix <- distance_matrix[7:134, 1:6]
	names(distance_matrix) <- Location_list
	# merge distance matrix with weather station info
	B <- dplyr::select(
		wstations, 
		Name, 
		Station.ID, 
		Latitude, 
		Longitude, 
		Precipitation, 
		Temperature, 
		Used_in_Analysis, 
		Sampling_Site, 
		Near_Sampling_Site, 
		Date_Range
	)
	distance_matrix %<>% cbind(B)
	# change distance matrix columns to one column
	weather_station_info <- reshape2::melt(
		distance_matrix, 
		id.vars=c(
			"Name", 
			"Station.ID", 
			"Latitude", 
			"Longitude", 
			"Precipitation", 
			"Temperature", 
			"Used_in_Analysis", 
			"Sampling_Site", 
			"Near_Sampling_Site", 
			"Date_Range"
		)
	)
	names(weather_station_info)[(dim(weather_station_info)[2]-1) : 
		dim(weather_station_info)[2]] <- c("Sampling_Location", "Distance")
	# standardize Station IDs - remove "GHCND:" if the the ID has it
	weather_station_info$Station.ID <- gsub(
		"^.*\\:", "", 
		weather_station_info$Station.ID
	)
	climate_data$STATION <- gsub("^.*\\:", "", climate_data$STATION)
	# SELECT CLOSEST WEATHER STATIONS FOR EACH SAMPLING LOCATION
	# merge distance data with climate_data
	climate_data_temp <- weather_station_info %>% 
		dplyr::select(Station.ID, Sampling_Location, Distance) %>%
		merge(climate_data, by.x="Station.ID", by.y="STATION")
	BLSP_stations <- climate_data_temp %>% 
		filter(Sampling_Location=="BLSP" & Distance <= Distance)
	HBSP_stations <- climate_data_temp %>% 
		filter(Sampling_Location=="HBSP" & Distance <= Distance) %>% 
		arrange(Date)
	MB_stations <- climate_data_temp %>% 
		filter(Sampling_Location=="MB" & Distance <= Distance)
	N_stations <- climate_data_temp %>% 
		filter(Sampling_Location=="N" & Distance <= Distance)
	SASP_stations <- climate_data_temp %>% 
		filter(Sampling_Location=="SASP" & Distance <= Distance)
	TSP_stations <- climate_data_temp %>% 
		filter(Sampling_Location=="TSP" & Distance <= Distance)
	# create list of climate stations per sampling location
	Dat = list(
		`climate_data`	= climate_data_temp,
		`BLSP_stations` = BLSP_stations,
		`HBSP_stations` = HBSP_stations,
		`MB_stations` 	= MB_stations,
		`N_stations` 	= N_stations,
		`SASP_stations` = SASP_stations,
		`TSP_stations` 	= TSP_stations
	)
	return(Dat)
}

#' Choose closest weather variable measurement for each Location/Date combo
#' @description For each date and location, get weather data from the closest available weather station.
#' @param Datalist Output (list format) from the \code{findClosestWeatherStations} function.
#' @export

getClimateDataByLocationDate <- function(Datalist) {
	X <- list()
	# for each LOCATION
	for (i in 1:length(Location_list)) {
		X[[i]] <- list()
		# pull climate data for that location and merge with station data to get distance from station to sampling location
		data = eval(parse(text=paste(
			"Datalist$", 
			Location_list[i], 
			"_stations", 
			sep=""
		)))
		# PRECIPITATION
		P 			<- data %>% filter(!is.na(Precip))
		X[[i]][[1]] <- as.data.frame(matrix(NA, length(unique(P$Date)), 6))
		names(X[[i]][[1]]) <- c(
			"Precip_STATION_NAME", 
			"Precip_STATION", 
			"Precip_STATION_Distance", 
			"Date", 
			"Precip", 
			"Location"
		)
		# for each DATE
		for (j in 1:length(unique(P$Date))) {
			# pull climate data associated with that location
			X[[i]][[1]][j, ] = P %>% 
				filter(Date == unique(Date)[j]) %>%
				filter(Distance == min(Distance)) %>%
				summarise(
					Precip_STATION_NAME 	= STATION_NAME,
					Precip_STATION 			= Station.ID,
					Precip_STATION_Distance = Distance,
					Date 					= Date[1],
					Precip 					= Precip
				)
		}
		# MIN TEMPERATURE
		Min 		<- data %>% filter(!is.na(MinTemp))
		X[[i]][[2]] <- as.data.frame(matrix(NA, length(unique(Min$Date)), 6))
		names(X[[i]][[2]]) <- c(
			"MinTemp_STATION_NAME", 
			"MinTemp_STATION", 
			"MinTemp_STATION_Distance", 
			"Date", 
			"MinTemp", 
			"Location"
		)
		# for each DATE
		for (j in 1:length(unique(Min$Date))) {
			# pull climate data associated with that location
			X[[i]][[2]][j, ] = Min %>% 
				filter(Date == unique(Date)[j]) %>%
				filter(Distance == min(Distance)) %>%
				summarise(
					MinTemp_STATION_NAME 		= STATION_NAME,
					MinTemp_STATION 			= Station.ID,
					MinTemp_STATION_Distance 	= Distance,
					Date 						= Date,
					MinTemp 					= MinTemp
				)
		}
		# MAX TEMPERATURE
		Max <- data %>% filter(!is.na(MaxTemp))
		X[[i]][[3]] <- as.data.frame(matrix(NA, length(unique(Max$Date)), 6))
		names(X[[i]][[3]]) <- c(
			"MaxTemp_STATION_NAME", 
			"MaxTemp_STATION", 
			"MaxTemp_STATION_Distance", 
			"Date", 
			"MaxTemp", 
			"Location"
		)
		# for each DATE
		for (j in 1:length(unique(Max$Date))) {
			# pull climate data associated with that location
			X[[i]][[3]][j, ] = Max %>% 
				filter(Date == unique(Date)[j]) %>%
				filter(Distance == min(Distance)) %>%
				summarise(
					MaxTemp_STATION_NAME 		= STATION_NAME,
					MaxTemp_STATION 			= Station.ID,
					MaxTemp_STATION_Distance 	= Distance,
					Date 						= Date,
					MaxTemp 					= MaxTemp
				)
		}
		X[[i]][[1]]$Date %<>% as.Date(origin="1970-01-01")
		X[[i]][[2]]$Date %<>% as.Date(origin="1970-01-01")
		X[[i]][[3]]$Date %<>% as.Date(origin="1970-01-01")
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
}

#' Calculate Growing Degree Days
#' @description I used this website to calculate growing degree days: http://www.ipm.ucdavis.edu/WEATHER/ddretrievetext.html. This function merges them together with the \code{climate_data} dataframe.
#' @param DegreeDay_list list of separate Degree Day files
#' @param climate_data climate data
#' @export

calculateDegreeDays <- function(climate_data, DegreeDay_list) {
	X <- list()
	for (i in 1:length(DegreeDay_list)) {
		X[[i]] <- eval(parse(text=DegreeDay_list[i]))
	}
	DegreeDays <- do.call(rbind.fill, X)
	DegreeDays$Date %<>% as.Date("%m/%d/%y")
	DegreeDays.unique = unique(DegreeDays[, 1:4])
	# DegreeDays %>% filter(Date=="2014-01-17")
	DegreeDays.merged = merge(
		DegreeDays.unique, 
		climate_data, 
		by.x=c("Date", "Temp.min", "Temp.max"), 
		by.y=c("Date", "MinTemp", "MaxTemp"), 
		all.y=T
	)
	names(DegreeDays.merged)[2:3] <- c("MinTemp", "MaxTemp")
	return(DegreeDays.merged)
}