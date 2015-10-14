#' Calculate log and sqrt transformations
#' @param data Vector of data
#' @param Name Name to give 
#' @description Transform variable.

Transform <- function(data, Name) {
	data %<>%
		mutate(
			sqrt(.[, 1])
		) %>%
		setnames(., dim(.)[2], paste(Name, "_sqrt", sep="")) %>%
		mutate(
			log(.[, 1])
		) %>%
		setnames(., dim(.)[2], paste(Name, "_log", sep=""))
	return(data)
}