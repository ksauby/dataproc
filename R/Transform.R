#' Calculate log and sqrt transformations
#'
#' @description Transform variable.
#' @param data Vector of data
#' @param Name Name to give 
#'
#' @export

Transform <- function(data, Name) {
	data %<>%
		mutate(
			sqrt(.data[, 1])
		) %>%
		setnames(.data, dim(.data)[2], paste("square root of ", Name, sep="")) %>%
		mutate(
			log(.data[, 1])
		) %>%
		setnames(.data, dim(.data)[2], paste("ln(", Name, ")", sep=""))
	return(data)
}