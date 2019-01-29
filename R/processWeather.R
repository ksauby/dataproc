#' Filter Data by Quality Flag
#' 
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
#' @param x Dataset with which to evaluate quality
#' @param y Dataset containing quality information
#'
#' @export

Quality_Flag_Function <- function(x, y){
	x[which(y=="G" | y=="I" | y=="K" | y=="L" | y=="N" | y=="O")] <- NA
	return(x)
}

#' Replace blank values (quality is okay) with "Okay"
#' 
#' @param x Dataset
#'
#' @export

Replace_Blank_w_Okay_Function <- function(x){
	x[which(x==" ")] <- "Okay"
	return(x)
}

#' Format Weather Station Info
#' 
#' @param wstations List of weather stations, downloaded from the NOAA NCDC site
#'
#' @export

formatWeatherStationInfo <- function(wstations, Start_Date = "2014-01-17", End_Date = "2008-01-20") {
	# create columns for Start and End Dates of Weather Station
	wstations$Start_Date <- sub(" .*", "", 
		wstations$Date_Range)
	wstations$End_Date <- sub(".* ", "", 
		wstations$Date_Range)
	# filter dates to study dates
	wstations %<>%
		filter(Start_Date <= Start_Date, End_Date >= End_Date) %>%
		# merge with list of weather stations for which start/end date is not known
		rbind(filter(wstations, Start_Date == "", End_Date >= ""))
	return(wstations)
}

#' Merge Weather Data Files and Format Column Names
#' 
#' @param climate_data Climate dataset
#' @description For each variable, NOAA uses generic column names "Measurement.Flag", "Quality.Flag", "Source.Flag", "Time.of.Observation" to ensure that the appropriate columns are merged together. This function renames these columns by pasting the name with the name of the variable to which it refers e.g., the "Measurement.Flag" column directly after "PRCP" will become "PRCP.Measurement.Flag" for each weather variable, take weather variable name and paste it to the names of the 4 following columns.
#'
#' @export
#' @importFrom plyr rbind.fill

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
			.data$STATION, 
			.data$STATION_NAME, 
			.data$ELEVATION, 
			.data$LATITUDE, 
			.data$LONGITUDE, 
			.data$DATE
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
#' 
#' @param climate_data Climate dataset
#' @description Replace data with NA if it is of questionable quality.
#' \itemize{ 
#' 		\item Filter data by quality
#' 		\itemize{
#' 			\item replace data with NA if it is of questionable quality (see \code{Quality_Flag_Function} function for details)
#' 			\item replace blank values (quality is okay) with "Okay"
#' 			\item Replace -999 and blanks with NAs
#' 		}
#' }
#'
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
	)] %<>% apply(.data, 2, Replace_Blank_w_Okay_Function)
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
	)] %<>% apply(.data, 2, NA_Function)
	return(climate_data)
}

#' Format and convert weather data
#' 
#' @param climate_data Climate dataset
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
#'
#' @export

formatconvertClimateData <- function(climate_data) {
	climate_data[,c(
		"PRCP",
		"TMIN",
		"TMAX"
	)] %<>% apply(.data, 2, as.numeric)
	climate_data %<>% mutate(
			MinTemp = .data$TMIN/10, # convert tenths of Celcius to Celsius
			MaxTemp = .data$TMAX/10, # convert tenths of Celcius to Celsius
			Precip = .data$PRCP/100, # convert PRCP (in tenths of mm) to cm
			Date = as.Date(as.character(.data$DATE), "%Y%m%d")
		)
	# replace NAs again just in case
	climate_data[,c("MinTemp","MaxTemp","Precip")] %<>% apply(.data, 2, NA_Function)
	# replace NA for precip
	climate_data$Precip[which(climate_data$Precip < 0)] <- NA
	return(climate_data)
}

#' Choose closest weather variable measurement for each Location/Date combo
#' 
#' @description For each date and location, get weather data from the closest available weather station.
#' @param Datalist Output (list format) from the \code{findClosestWeatherStations} function.
#'
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
		X[[i]][[2]] <- as.data.frame(
			matrix(NA, length(unique(Min$Date)), 6)
		)
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
		X[[i]][[3]] <- as.data.frame(
			matrix(NA, length(unique(Max$Date)), 6)
		)
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
#' 
#' @description I used this website to calculate growing degree days: http://www.ipm.ucdavis.edu/WEATHER/ddretrievetext.html. This function merges them together with the \code{climate_data} dataframe.
#' @param DegreeDay_list list of separate Degree Day files
#' @param climate_data Climate dataset
#'
#' @export

calculateDegreeDays <- function(climate_data, DegreeDay_list) {
	X <- list()
	for (i in 1:length(DegreeDay_list)) {
		X[[i]] <- eval(parse(text=DegreeDay_list[i]))
	}
	DegreeDays <- do.call(rbind.fill, X)
	DegreeDays$Date %<>% as.Date("%Y-%m-%d")
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



#' Create Year and Day of Year Variables for Climate Data
#' 
#' @param climate_data climate data
#' @description Create Year and Day of Year variables.
#'
#' @export

formatClimateDataYearDayofYear <- function(climate_data) {
	climate_data %>% 
		mutate(
			Year = year(Date),
			Day_of_year = as.numeric(strftime(Date, format = "%j"))
		)			
}

#' Calculate climate variables from weather data
#' 
#' @param x survey data
#' @param climate_data climate data
#' @param calculate_dates Default is \code{TRUE}. Either \code{x} is a dataframe of survey dates from which to calculate dates for climate variables or \code{x} is a list of pre-determined dates and their locations, in which case \code{calculate_dates} should be \code{FALSE}.
#'
#' @export
#' @importFrom dplyr group_by summarise arrange filter


calculateClimateVariables <- function(x, climate_data, calculate_dates="TRUE", Dates_dataframe=NULL, first.year=2009) {
	climate_data %<>% 
		renameLocations(.data) %>%
		formatClimateDataYearDayofYear(.data) %>%
		mutate(
			Precip_Presence = ifelse(.data$Precip>0, 1, 0),
			MinTemp_lt_equal_0 = ifelse(.data$MinTemp<=0, 1, 0)
		)			
	# create list of dates
	if (calculate_dates=="TRUE") {
		# get unique Date and DaysSincePrevSurvey combos
		A = as.data.frame(x) %>%
			group_by(.data$Date, .data$Location, .data$Species) %>%
			# to make sure that this group all share the same Previous_Survey_Date Date
			summarise(PrevSurvD = paste(max(.data$Previous_Survey_Date, na.rm=T))) %>%
			as.data.frame %>%
			arrange(.data$Location, .data$Date)
		#----------------- Fill in Missing Previous Survey Date for Survey 1 --#
		# Generate fake "PrevSurvD" based on the average number of days 
		#		between surveys
		A$PrevSurvD[A$PrevSurvD=="NA" & year(A$Date)==first.year] <- 
			as.character(A$Date[A$PrevSurvD=="NA" & year(A$Date)==first.year] - 
			round(mean(x$DaysSincePrevSurvey, na.rm=T)))
		A 			%<>% filter(PrevSurvD!="NA")
		A$PrevSurvD %<>% as.Date("%Y-%m-%d")
	} else {
		A <- Dates_dataframe
		# A %<>% mutate(
			# previous dates
	#		PrevSurvD = as.Date(c(NA, Date[-length(Date)]), origin="1970-01-01")
		#)
	}
	# prep dataframe for addition of of climate variables
	Names <- c(
		"Daily_Precip_mean",
		"Daily_Precip_SD",
		"Perc_Days_w_Rain",
		"Perc_Days_w_Freeze",
		"mean_Max_Temp",
		"sd_Max_Temp",
		"MeanDegreeDay",
		"Mean_Consecutive_Days_w_Rain",
		"Max_Consecutive_Days_w_Rain",
		"sd_Consecutive_Days_w_Rain",
		"Mean_Consecutive_Drought_Days",
		"Max_Consecutive_Drought_Days",
		"sd_Consecutive_Drought_Days",
		"Mean_Consecutive_Freezing_Days",
		"Max_Consecutive_Freezing_Days",
		"sd_Consecutive_Freezing_Days"
	)
	A1 <- A %>% cbind(
		as.data.frame(
			matrix(
				nrow=dim(A)[1], 
				ncol=length(Names),
				data=NA
			)
		)
	)
	names(A1)[ (dim(A)[2] + 1) : (dim(A)[2] + length(Names)) ] <- Names
	# for new plants, assign previous visit based on previous visit to location
	# calculate climate variables
	for (i in 1:dim(A1)[1]) {
		# subset data by time period and location
		#		date is equal to or greater than previous survey date
		temp = climate_data[which(
			climate_data$Date >= A1$PrevSurvD[i] & 
			climate_data$Date < A1$Date[i] & # date is less than current date
			climate_data$Location==A1$Location[i]
		), ] # and pull data for correct location 
		# Climate Variable Calculations
		A1$Daily_Precip_mean[i] 	<- mean(temp$Precip, na.rm=T)
		A1$Daily_Precip_SD[i] 	<- sd(temp$Precip, na.rm=T)
		#		length of precip > 0 / length of precip != NA
		A1$Perc_Days_w_Rain[i] 	<- 
			length(filter(temp, Precip>0)$Precip) / length(!is.na(temp$Precip))
		#		length of MinTemp <= 0 / length of MinTemp != NA
		A1$Perc_Days_w_Freeze[i] <- 
			length(filter(temp, MinTemp<=0)$MinTemp) / 
			length(!is.na(temp$MinTemp))
		A1$mean_Max_Temp[i] 		<- mean(temp$MaxTemp, na.rm=T)
		A1$sd_Max_Temp[i] 		<- sd(temp$MaxTemp, na.rm=T)
		A1$MeanDegreeDay[i] 		<- mean(temp$Daily.DD, na.rm=T)
		# consecutive days with rain
		# count identical consecutive values
		r <- rle(temp$Precip_Presence)
		# get lengths for value=1
		r1 <- r$length[r$values == 1]
		A1$Mean_Consecutive_Days_w_Rain[i] 	<- mean(r1, na.rm=T)
		A1$Max_Consecutive_Days_w_Rain[i] 	<- max(r1, na.rm=T)
		A1$sd_Consecutive_Days_w_Rain[i] 	<- sd(r1, na.rm=T)
		# consecutive drought days
		r0 <- r$length[r$values == 0]
		A1$Mean_Consecutive_Drought_Days[i] 	<- mean(r0, na.rm=T)
		A1$Max_Consecutive_Drought_Days[i] 	<- max(r0, na.rm=T)
		A1$sd_Consecutive_Drought_Days[i] 	<- sd(r0, na.rm=T)
		# Consecutive Freezing Days
		# count identical consecutive values
		r <- rle(temp$MinTemp_lt_equal_0)
		# get lengths for value=1
		r1 <- r$length[r$values == 1]
		#	if there was at least one freezing day:
		if (length(r1) > 0) {
			A1$Mean_Consecutive_Freezing_Days[i] 	<- mean(r1, na.rm=T)
			A1$Max_Consecutive_Freezing_Days[i] 		<- max(r1, na.rm=T)
			# 	calculate sd if there is more than one value; otherwise sd=0
			A1$sd_Consecutive_Freezing_Days[i] 		<- ifelse(
				length(r1)>1, 
				sd(r$length[r$values == 1], na.rm=T),
				0
			)
		} else {
			A1$Mean_Consecutive_Freezing_Days[i] 	<- 0
			A1$Max_Consecutive_Freezing_Days[i] 		<- 0
			A1$sd_Consecutive_Freezing_Days[i] 		<- 0
		}
	}
	# replace NAs
	A1 %<>% as.data.frame
	A1[,c(
		"Daily_Precip_mean",
		"Daily_Precip_SD",
		"Perc_Days_w_Rain",
		"Perc_Days_w_Freeze",
		"mean_Max_Temp",
		"sd_Max_Temp",
		"MeanDegreeDay",
		"Mean_Consecutive_Days_w_Rain",
		"Max_Consecutive_Days_w_Rain",
		"sd_Consecutive_Days_w_Rain",
		"Mean_Consecutive_Drought_Days",
		"Max_Consecutive_Drought_Days",
		"sd_Consecutive_Drought_Days",
		"Mean_Consecutive_Freezing_Days",
		"Max_Consecutive_Freezing_Days",
		"sd_Consecutive_Freezing_Days")] %<>% 
		apply(.data, 2, NA_Function
	)
	A1 %<>% dplyr::select(-.data$PrevSurvD)
	return(A1)
}

#' Fix Erroneous Temperature Data
#' 
#' @param climate_data climate data
#' @description If a date's minimum temperature record is greater than or equal to the maximum temperature record, replace both values with NA.
#'
#' @export

fixErroneousTemps <- function(climate_data) {
	X = as.vector(which(climate_data$MinTemp >= climate_data$MaxTemp))
	if (!is.null(dim(X))) {
		climate_data[X, ]$MinTemp <- NA
		climate_data[X, ]$MaxTemp <- NA
	}
	return(climate_data)
}