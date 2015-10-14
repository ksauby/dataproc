#' Process PCA Precipitation data before merging with survey data.
#' @param x Output PCA data (from SAS).
#' @description For use with SAS data.

process_precip_PCA_data <- function(data) {
	data$Date %<>% as.Date
	data %>%
		setnames("Factor1", "P1") %>%
		setnames("Factor2", "P2") %>%
		as.data.frame %>%
		dplyr::select(-c(A1, A2, B, C1, C2, C3, D1, D2, D3))
}