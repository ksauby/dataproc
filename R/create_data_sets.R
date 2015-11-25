#' Create Dataset with All Surveys
#' @description Create dataset from all surveys and calculate:
#' \itemize{
#'  \item lagged fruit values
#'  \item lagged dates
#'  \item lagged insect presence
#'  \item relative growth rate
#' }
#' @param timeseries
#' @export

createAllSurveysDataset <- function(timeseries) {
	timeseries_all_surveys <- timeseries
	timeseries_all_surveys %<>% 
								# lag variables
								lag_size_fruit_function %>%
								lag_dates_function %>%
								lag_insects_function %>%
								# RGR
								RGR_function
	# Save
	setwd("/Users/KSauby/Documents/Dropbox/GradSchool/Research/Projects/marsico-time-series/")
	cache("timeseries_all_surveys")
	return(timeseries_all_surveys)
}	
	
#' Create Dataset with Fall/Winter Surveys
#' @description Create dataset from fall/winter surveys and calculate:
#' \itemize{
#'  \item lagged fruit values
#'  \item lagged dates
#'  \item lagged insect presence
#'  \item relative growth rate
#' }
#' @param timeseries
#' @export
	
createFallWinterSurveysDataset <- function(timeseries) {
	timeseries_winter <- timeseries
	timeseries_winter$Visit <- "NA"
	timeseries_winter %<>% 
		as.data.frame %>%
		group_by(Date) %>% 
		mutate(
			Visit = replace(Visit, which(Date >= "2009-01-21" & 
				Date < "2010-11-15"), "Year2010"),
			Visit = replace(Visit, which(Date >= "2010-11-15" & 
				Date < "2011-12-15"), "Year2011"),
			Visit = replace(Visit, which(Date >= "2011-12-15" & 
				Date < "2012-12-14"), "Year2012"),
			Visit = replace(Visit, which(Date >= "2012-12-14" & 
				Date < "2014-01-12"), "Year2014"))
	new = timeseries_winter %>%
		select(Visit, PlantID, ME_t, CA_t, CH_t, DA_t) %>%
		group_by(Visit, PlantID) %>%
		summarise(
			CAyr_t 			= ifelse(sum(CA_t, na.rm=T)>0, 1, 0),
			MEyr_t 			= ifelse(sum(ME_t, na.rm=T)>0, 1, 0),
			CHyr_t 			= ifelse(sum(CH_t, na.rm=T)>0, 1, 0),
			DAyr_t 			= ifelse(sum(DA_t, na.rm=T)>0, 1, 0),
			Insectyr_t 		= ifelse(
				sum(DA_t, CA_t, CH_t, ME_t, na.rm=T) > 0, 1, 0),
			NatInsectyr_t 	= ifelse(sum(DA_t, CH_t, ME_t, na.rm=T)>0, 1, 0)
		)
	# use only winter data
	timeseries_winter %<>%
		mutate(Month=month(Date)) %>%
		arrange(Date) %>%
		filter(Month==10 | Month==11 | Month==12 | Month==1) %>%
		merge(new, by=c("Visit", "PlantID"), all.x=T)
	# create lag and RGR variables
	timeseries_winter %<>%
						# lag variables
						lag_size_fruit_function %>%
						lag_dates_function %>%
						lag_insects_yr_function %>%
						# RGR
						as.data.frame %>%
						RGR_function
	# Save
	setwd("/Users/KSauby/Documents/Dropbox/GradSchool/Research/Projects/marsico-time-series/")
	cache("timeseries_winter")
	return(timeseries_winter)
}	
	
	
	
	
#' Create Dataset with Spring/Summer Surveys
#' @description Create dataset from spring/summer surveys and calculate:
#' \itemize{
#'  \item lagged fruit values
#'  \item lagged dates
#'  \item lagged insect presence
#'  \item relative growth rate
#' }
#' @param timeseries
#' @export
	
createSpringSummerSurveysDataset <- function(timeseries) {
	timeseries_spring <- timeseries
	timeseries_spring$Visit <- "NA"
	timeseries_spring %<>% 
		as.data.frame %>%
		group_by(Date) %>% 
		mutate(
			Visit = replace(Visit, which(Date >= "2009-04-01" & 
				Date < "2010-05-01"), "Year1"),
			Visit = replace(Visit, which(Date >= "2010-05-01" & 
				Date < "2011-05-01"), "Year2"),
			Visit = replace(Visit, which(Date >= "2011-05-01" & 
				Date < "2012-05-01"), "Year3"),
			Visit = replace(Visit, which(Date >= "2012-05-01" & 
				Date < "2013-05-01"), "Year4"),
			Visit = replace(Visit, which(Date > "2013-05-01" ), "Year5"))
	new = timeseries_spring %>%
		select(Visit, PlantID, ME_t, CA_t, CH_t, DA_t) %>%
		group_by(Visit, PlantID) %>%
		summarise(
			CAyr_t 			= ifelse(sum(CA_t, na.rm=T)>0, 1, 0),
			MEyr_t 			= ifelse(sum(ME_t, na.rm=T)>0, 1, 0),
			CHyr_t 			= ifelse(sum(CH_t, na.rm=T)>0, 1, 0),
			DAyr_t 			= ifelse(sum(DA_t, na.rm=T)>0, 1, 0),
			Insectyr_t 		= ifelse(
				sum(DA_t, CA_t, CH_t, ME_t, na.rm=T) > 0, 1, 0),
			NatInsectyr_t 	= ifelse(sum(DA_t, CH_t, ME_t, na.rm=T)>0, 1, 0)
		)
	# use only spring data
	timeseries_spring %<>%
		mutate(Month=month(Date)) %>%
		arrange(Date) %>%
		filter(Month==4 | Month==5) %>%
		merge(new, by=c("Visit", "PlantID"), all.x=T)
	# create lag and RGR variables
	timeseries_spring %<>%
						# lag variables
						lag_size_fruit_function %>%
						lag_dates_function %>%
						lag_insects_yr_function %>%
						# RGR
						as.data.frame %>%
						RGR_function
	# Save
	setwd("/Users/KSauby/Documents/Dropbox/GradSchool/Research/Projects/marsico-time-series/")
	cache("timeseries_spring")
	return(timeseries_spring)
}