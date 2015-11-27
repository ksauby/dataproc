#' Calculate log and sqrt transformations
#' @description Transform variable.
#' @param data Vector of data
#' @param Name Name to give 
#' @export

Transform <- function(data, Name) {
	data %<>%
		mutate(
			sqrt(.[, 1])
		) %>%
		setnames(., dim(.)[2], paste("square root of ", Name, sep="")) %>%
		mutate(
			log(.[, 1])
		) %>%
		setnames(., dim(.)[2], paste("ln(", Name, ")", sep=""))
	return(data)
}