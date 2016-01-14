#' Process PCA Precipitation data before merging with survey data.
#'
#' @param x Output PCA data (from SAS).
#' @description For use with SAS data.
#'
#' @export

process_precip_PCA_data <- function(data) {
	data$Location %<>% factor
	data$Date %<>% as.Date
	data %>%
		setnames("Factor1", "P1") %>%
		setnames("Factor2", "P2") %>%
		as.data.frame
}

#' Process Fall/Winter PCA Precipitation data before merging with survey data.
#'
#' @param x Output PCA data (from SAS).
#' @description For use with SAS data.
#'
#' @export

process_FWprecip_PCA_data <- function(data) {
	data %<>% mutate(ObsYear = year(Date) - 1)
	data$Location %<>% factor
	data$Date %<>% as.Date
	data %>%
		setnames("Factor1", "P1_FW") %>%
		setnames("Factor2", "P2_FW") %>%
		as.data.frame
}

#' Process Spring/Summer PCA Precipitation data before merging with survey data.
#'
#' @param x Output PCA data (from SAS).
#' @description For use with SAS data.
#'
#' @export

process_SSprecip_PCA_data <- function(data) {
	data %<>% mutate(ObsYear = year(Date))
	data$Location %<>% factor
	data$Date %<>% as.Date
	data %>%
		setnames("Factor1", "P1_SS") %>%
		setnames("Factor2", "P2_SS") %>%
		as.data.frame
}