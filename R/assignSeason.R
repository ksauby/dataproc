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
#' @importFrom rlang .data


assignSeason <- function(dat, SeasonStarts) {
	dat %>% 
		rowwise(.data) %>%
		mutate(
		Season = lapply(.data$Date,
				function(x) {
					findInterval(
						x, 
						SeasonStarts[which(year(x)==year(SeasonStarts$WS)), ]
					)
				}
		) %>% unlist,
		Season = replace(
		.data$Season,
			which(.data$Season==0 | .data$Season==4),
			"Winter"
		),
		Season = replace(
			.data$Season,
			which(.data$Season==1),
			"Spring"
		),
		Season = replace(
			.data$Season,
			which(.data$Season==2),
			"Summer"
		),
		Season = replace(
			.data$Season,
			which(.data$Season==3),
			"Fall"
		)
	)
}