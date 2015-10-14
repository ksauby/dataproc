#' Process PCA Temperature data before merging with survey data.
#' @param x Output PCA data (from SAS).
#' @description For use with SAS data.

process_temp_PCA_data <- function(data) {
	data$Date %<>% as.Date
	data %>%
		setnames("Factor1", "P1") %>%
		setnames("Factor2", "P2") %>%
		as.data.frame %>%
		dplyr::select(-c(G, E1, E2, F, H1, H2, H3))
}
