weather_function <- function(x=x, climate_data=climate_data){
	# get unique Date and DaysSincePrevSurvey combos
	# to reduce number of calculations to get sum of precip/number of days with precip
	if ("Mexico Beach" %in% climate_data$Location) {
		climate_data[which(climate_data$Location=="Mexico Beach"), ]$Location <- 
			"MB"
		climate_data[which(climate_data$Location=="Nokuse"), ]$Location <- 
			"N"	
		climate_data[which(climate_data$Location=="Sweetwater"), ]$Location <- 
			"TSP"
	}
	A = as.data.frame(x) %>%
		group_by(Date, Location, Species) %>%
		# to make sure that this group all share the same Previous_Survey_Date Date
		summarise(
			PrevSurvD = paste(Maximum(Previous_Survey_Date))
		) %>%
		as.data.frame %>%
		arrange(Location, Date)
	#-------------------- Fill in Missing Previous Survey Date for Survey 1 --#
	# Generate fake "PrevSurvD" based on the average number of days 
	#		between surveys
	A$PrevSurvD[A$PrevSurvD=="NA" & year(A$Date)==2009] <- 
		as.character(A$Date[A$PrevSurvD=="NA" & year(A$Date)==2009] - 
		round(mean(x$DaysSincePrevSurvey, na.rm=T)))
	A %<>% filter(PrevSurvD!="NA")
	A$PrevSurvD %<>% as.Date("%Y-%m-%d")
	Names = c("Daily_Precip_mean",
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
	A %<>% cbind(as.data.frame(matrix(nrow=dim(A)[1], ncol=length(Names),
		data=NA)))
	names(A)[5:(4+length(Names))] <- Names
	# for new plants, assign previous visit based on previous visit to location
	# calculate climate variables
	for (i in 1:dim(A)[1]) {
		# subset data by time period and location
		#		date is equal to or greater than previous survey date
		temp = climate_data[which(climate_data$Date >= A$PrevSurvD[i] & 
			climate_data$Date < A$Date[i] & # date is less than current date
			climate_data$Location==A$Location[i]), ] # and pull data for correct location 
		# Climate Variable Calculations
		A$Daily_Precip_mean[i] <- mean(temp$Precip, na.rm=T)
		A$Daily_Precip_SD[i] <- sd(temp$Precip, na.rm=T)
		#		length of precip > 0 / length of precip != NA
		A$Perc_Days_w_Rain[i] = 
			length(filter(temp, Precip>0)$Precip) / length(!is.na(temp$Precip))
		#		length of MinTemp <= 0 / length of MinTemp != NA
		A$Perc_Days_w_Freeze[i] = 
			length(filter(temp, MinTemp<=0)$MinTemp) / 
			length(!is.na(temp$MinTemp))
		A$mean_Max_Temp[i] = mean(temp$MaxTemp, na.rm=T)
		A$sd_Max_Temp[i] = sd(temp$MaxTemp, na.rm=T)
		A$MeanDegreeDay[i] = mean(temp$Daily.DD, na.rm=T)
		# consecutive days with rain
			# count identical consecutive values
			r = rle(temp$Precip_Presence)
			# get lengths for value=1
			A$Mean_Consecutive_Days_w_Rain[i] = 
				mean(r$length[r$values == 1], na.rm=T)
			A$Max_Consecutive_Days_w_Rain[i] = 
				max(r$length[r$values == 1], na.rm=T)
			A$sd_Consecutive_Days_w_Rain[i] = 
				sd(r$length[r$values == 1], na.rm=T)
		# consecutive drought days
			A$Mean_Consecutive_Drought_Days[i] = 
				mean(r$length[r$values == 0], na.rm=T)
			A$Max_Consecutive_Drought_Days[i] = 
				max(r$length[r$values == 0], na.rm=T)
			A$sd_Consecutive_Drought_Days[i] = 
				sd(r$length[r$values == 0], na.rm=T)
		# Consecutive Freezing Days
			# count identical consecutive values
			r = rle(temp$MinTemp_lt_equal_0)
			# get lengths for value=1
			#	if there was at least one freezing day:
			if (length(r$length[r$values == 1]) > 0) {
				A$Mean_Consecutive_Freezing_Days[i] = 
					mean(r$length[r$values == 1], na.rm=T)
				A$Max_Consecutive_Freezing_Days[i] = 
					max(r$length[r$values == 1], na.rm=T)
				# 	calculate sd if there is more than one value; otherwise sd=0
				A$sd_Consecutive_Freezing_Days[i] = 
					ifelse(length(r$length[r$values == 1])>1, 
					sd(r$length[r$values == 1], na.rm=T), 0)
			} else {
				A$Mean_Consecutive_Freezing_Days[i] = 0
				A$Max_Consecutive_Freezing_Days[i] = 0
				A$sd_Consecutive_Freezing_Days[i] = 0
			}
	}
	# replace NAs
	A %<>% as.data.frame
	A[,c(
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
		apply(., 2, NA_Function
	)
	A %<>% select(-PrevSurvD)
	return(A)
}
