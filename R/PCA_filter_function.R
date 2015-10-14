#' filter data to make PCA
#' @param x
#' @param species
#' @param restriction_list
PCA_filter_function <- function(x=x, species=species, restriction_list=restriction_list){
	x %>%
		as.data.frame %>%
		filter(Species==species) %>%
		.[complete.cases(.), ]
}
