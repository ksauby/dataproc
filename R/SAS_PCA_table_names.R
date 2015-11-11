#' Change PCA Table names
#' @param table created with the \code{sas_prcomp_PCA_table_function} function.
#' @description First create a table from SAS output, then use this function to change the values of a couple of columns.


sas_PCA_table_names <- function(table) {
	table %<>% 
		mutate(`Summary Statistic` = Var) %>%
		mutate(
			`Summary Statistic` = replace(
				`Summary Statistic`, 
				which(`Summary Statistic`=="A1"), 
				"Mean"
			),
			`Summary Statistic` = replace(
				`Summary Statistic`, 
				which(`Summary Statistic`=="A2"), 
				"SD"
			),
			`Summary Statistic` = replace(
				`Summary Statistic`, 
				which(`Summary Statistic`=="B"), 
				""
			),
			`Summary Statistic` = replace(
				`Summary Statistic`, 
				which(`Summary Statistic`=="C1"), 
				"Mean"
			),
			`Summary Statistic` = replace(
				`Summary Statistic`, 
				which(`Summary Statistic`=="C2"), 
				"Max."
			),
			`Summary Statistic` = replace(
				`Summary Statistic`, 
				which(`Summary Statistic`=="C3"), 
				"SD"
			),
			`Summary Statistic` = replace(
				`Summary Statistic`, 
				which(`Summary Statistic`=="D1"), 
				"Mean"
			),
			`Summary Statistic` = replace(
				`Summary Statistic`, 
				which(`Summary Statistic`=="D2"), 
				"Max."
			),
			`Summary Statistic` = replace(
				`Summary Statistic`, 
				which(`Summary Statistic`=="D3"), 
				"SD"
			),
			`Summary Statistic` = replace(
				`Summary Statistic`, 
				which(`Summary Statistic`=="E1"), 
				"Mean"
			),
			`Summary Statistic` = replace(
				`Summary Statistic`, 
				which(`Summary Statistic`=="E2"), 
				"SD"
			),
			`Summary Statistic` = replace(
				`Summary Statistic`, 
				which(`Summary Statistic`=="F"), 
				"Mean"
			),
			`Summary Statistic` = replace(
				`Summary Statistic`, 
				which(`Summary Statistic`=="G"), 
				""
			),
			`Summary Statistic` = replace(
				`Summary Statistic`, 
				which(`Summary Statistic`=="H1"), 
				"Mean"
			),
			`Summary Statistic` = replace(
				`Summary Statistic`, 
				which(`Summary Statistic`=="H2"), 
				"Max."
			),
			`Summary Statistic` = replace(
				`Summary Statistic`, 
				which(`Summary Statistic`=="H3"), 
				"SD"
			),	
			`Summary Statistic` = replace(
				`Summary Statistic`, 
				which(`Summary Statistic`=="Eigenvalue"), 
				""
			),	
			`Summary Statistic` = replace(
				`Summary Statistic`, 
				which(`Summary Statistic`=="Cumulative Proportion of Variance Explained"), 
				""
			),	
			`Summary Statistic` = replace(
				`Summary Statistic`, 
				which(`Summary Statistic`=="Rotated"), 
				""
			)
		) %>%
	#	mutate(
	#		`All Data PC 2` = replace(
	#			`All Data PC 2`, 
	#			which(is.na(`All Data PC 2`)), 
	#			""
	#		)
	#	) %>%
		mutate(
			Var = replace(
				Var, 
				which(Var=="A1"), 
				"Daily Precipitation"
			),
			Var = replace(
				Var, 
				which(Var=="A2"), 
				""
			),
			Var = replace(
				Var, 
				which(Var=="B"), 
				"Percentage of Days with Rain"
			),
			Var = replace(
				Var, 
				which(Var=="C1"), 
				"Number of Consecutive Days with Rain"
			),
			Var = replace(
				Var, 
				which(Var=="C2"), 
				""
			),
			Var = replace(
				Var, 
				which(Var=="C3"), 
				""
			),
			Var = replace(
				Var, 
				which(Var=="D1"), 
				"Number of Consecutive Days without Rain"
			),
			Var = replace(
				Var, 
				which(Var=="D2"), 
				""
			),
			Var = replace(
				Var, 
				which(Var=="D3"), 
				""
			),
			Var = replace(
				Var, 
				which(Var=="E1"), 
				"Maximum Temperature"
			),
			Var = replace(
				Var, 
				which(Var=="E2"), 
				""
			),
			Var = replace(
				Var, 
				which(Var=="F"), 
				"Mean Degree Day"
			),
			Var = replace(
				Var, 
				which(Var=="G"), 
				"Percentage of Freezing Days"),
			Var = replace(
				Var, 
				which(Var=="H1"), 
				"Number of Cosecutive Days with Temperatures Below Freezing"
			),
			Var = replace(
				Var, 
				which(Var=="H2"), 
				""
			),
			Var = replace(
				Var, 
				which(Var=="H3"), 
				""
			)
		) %>%
		dplyr::select(Var, `Summary Statistic`, everything())
	names(table)[names(table)=="Var"] <- "Variable"		
	return(table)
}