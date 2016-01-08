#' Process PCA Temperature data before merging with survey data.
#' @param x Output PCA data (from SAS).
#' @description For use with SAS data.
#' @export

process_temp_PCA_data <- function(data) {
	data$Date %<>% as.Date
	data$Location %<>% factor
	data %>%
		setnames("Factor1", "T1") %>%
		setnames("Factor2", "T2") %>%
		as.data.frame
}

#' Process Fall/Winter PCA Temperature data before merging with survey data.
#' @param x Output PCA data (from SAS).
#' @description For use with SAS data.
#' @export

process_FWtemp_PCA_data <- function(data) {
	data %<>% mutate(ObsYear = year(Date) - 1)
	data$Date %<>% as.Date
	data$Location %<>% factor
	data %>%
		setnames("Factor1", "T1_FW") %>%
		setnames("Factor2", "T2_FW") %>%
		as.data.frame
}

#' Process Spring/Summer PCA Temperature data before merging with survey data.
#' @param x Output PCA data (from SAS).
#' @description For use with SAS data.
#' @export

process_SStemp_PCA_data <- function(data) {
	data %<>% mutate(ObsYear = year(Date))
	data$Date %<>% as.Date
	data$Location %<>% factor
	data %>%
		setnames("Factor1", "T1_SS") %>%
		setnames("Factor2", "T2_SS") %>%
		as.data.frame
}
