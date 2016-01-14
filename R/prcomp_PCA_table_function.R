#' Build a table of PCA results.
#'
#' @param x output of the \code{prcomp} function.
#' @description Label to append to PC 1 and PC 2 column headers; particularly useful if one wants to display data from multiple PCAs.
#'
#' @export
#' @import data.table

prcomp_PCA_table_function <- function(x, data.type) {
	y = x$rotation[,1:2] %>%
		as.data.frame %>%
		round(3) %>%
		dplyr::mutate(
			V = rownames(.)
		) %>%
		select(V, everything()) %>%
		rbind(c(
			"Eigenvalue", 
			x$sdev[1:2] %>% round(3)
		)) %>%
		rbind(c(
			"Proportion of Variance Explained",
			summary(x)$importance[2, 1:2] %>% round(3)
		)) %>%
		setnames("PC1", paste(data.type, "PC1")) %>%
		setnames("PC2", paste(data.type, "PC2"))		
	return(y)
}