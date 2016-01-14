#' Assign Season based on Date
#' 
#' @description Assign seasons based on date.
#' @param dat dataframe including "Date" in \code{POSIXct} format.
#' @examples
#' dat = data.frame(
#'     Date = as.POSIXct(strptime(as.Date("2011-12-01", format = "%Y-%m-%d") + 
#'         (0:10)*30, format="%Y-%m-%d"))
#' )
#' dat %>% assignSeason
#' 
#' @export
#' @importFrom lubridate year


assignSeason <- function(dat, SeasonStarts=seasons) {
	dat %<>% mutate(
		Season = lapply(Date,
				function(x) {
					findInterval(
						x, 
						SeasonStarts[which(year(x)==year(SeasonStarts$WS)), ]
					)
				}
			) %>% unlist	
		)
	dat[which(dat$Season==0 | dat$Season==4), ]$Season 	<- "Winter"
	dat[which(dat$Season==1), ]$Season 					<- "Spring"
	dat[which(dat$Season==2), ]$Season 					<- "Summer"
	dat[which(dat$Season==3) ,]$Season 					<- "Fall"
	return(dat)
}