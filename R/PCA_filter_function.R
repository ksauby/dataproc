#' filter data to make PCA
#' 
#' @param x Dataset
#' @param species The species to which the data should be restricted
#' @description Filter data and get complete cases to prepare data  for PCA.
#' @export

PCA_filter_function <- function(x=x, species=species){
	x %>%
		as.data.frame %>%
		filter(Species==species) %>%
		.[complete.cases(.), ]
}
