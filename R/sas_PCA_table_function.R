#' Combine PCA results from SAS into a table
#'
#' @param eigens SAS output of eigenvalues and variance explained
#' @param factors SAS output of factor pattern
#' @param rfactors SAS output of rotated factor pattern
#' @param dataset_type Default value is "all"
#' @param round_n Default value is 3.
#' @param n.axes Default value is 2.
#' @description Process PCA results from SAS.
#'
#' @export
#' @importFrom magrittr %>% %<>% %$%
#' @importFrom dplyr filter mutate desc
#' @importFrom plyr rbind.fill
#' @importFrom reshape2 dcast

sas_PCA_table_function <- function(eigens, factors, rfactors, dataset_type="all", round_n=3, n.axes=2) {
	# which models are NOT in the rotated factor pattern data? merge them with the rotated factor pattern data
	factors %<>% 
		filter(!(modelVars %in% rfactors$modelVars)) %>%
		mutate(Rotated = "No")
	# if there are PCA analyses with unrotated axesm, merge with the rotated axis data	
	if (dim(factors)[1] > 0) {
		X <- rfactors %>%
			mutate(Rotated = "Yes") %>%
			rbind.fill(factors)
	} else {
		X <- rfactors %>%
			mutate(Rotated = "Yes")
	}	
	# square the factor loadings
	# X %<>%
	# mutate(
	#	Factor1 = Factor1^2,
	#	Factor2 = Factor2^2
	# )
	# round numeric values
	nums <- sapply(X, is.numeric)
	X[, nums] %<>% round(round_n)
	nums <- sapply(eigens, is.numeric)
	eigens[, nums] %<>% round(round_n)
	# edit cells so they indicate transformations
	Y <- X %>% 
		# log
		mutate(
			Factor1 = ifelse(
				substr(Variable,1,3)=="log",
				paste(Factor1, "(log)"),
				Factor1
			),
			Factor2 = ifelse(
				substr(Variable,1,3)=="log",
				paste(Factor2, "(log)"),
				Factor2
			),
			Variable = ifelse(
				substr(Variable,1,3)=="log",
				substring(Variable, 4),
				Variable
			)
		) %>%
		# 1.25 power
		mutate(
			Factor1 = ifelse(
				str_sub(Variable, -3, -1)=="125",
				paste(Factor1, "(^1.25)"),
				Factor1
			),
			Factor2 = ifelse(
				str_sub(Variable, -3, -1)=="125",
				paste(Factor2, "(^1.25)"),
				Factor2
			),
			Variable = ifelse(
				str_sub(Variable, -3, -1)=="125",
				gsub('.{3}$', '', Variable),
				#substr(Variable,1,nchar(Variable)-3), # remove last three characters
				Variable
			)
		) %>%	
		# 0.75 power
		mutate(
			Factor1 = ifelse(
				str_sub(Variable, -3, -1)=="075",
				paste(Factor1, "(^0.75)"),
				Factor1
			),
			Factor2 = ifelse(
				str_sub(Variable, -3, -1)=="075",
				paste(Factor2, "(^0.75)"),
				Factor2
			),
			Variable = ifelse(
				str_sub(Variable, -3, -1)=="075",
				gsub('.{3}$', '', Variable),
				Variable
			)
		) %>%
		# sqrt
		mutate(
			Factor1 = ifelse(
				substr(Variable,1,3)=="sqr",
				paste(Factor1, "(sqrt)"),
				Factor1
			),
			Factor2 = ifelse(
				substr(Variable,1,3)=="sqr",
				paste(Factor2, "(sqrt)"),
				Factor2
			),
			Variable = ifelse(
				substr(Variable,1,3)=="sqr",
				substring(Variable, 4),
				Variable
			)
		) %>%
		# ^2
		mutate(
			Factor1 = ifelse(
				substrRight(Variable,3)=="sqr",
				paste(Factor1, "(^2)"),
				Factor1
			),
			Factor2 = ifelse(
				substrRight(Variable,3)=="sqr",
				paste(Factor2, "(^2)"),
				Factor2
			),
			Variable = ifelse(
				substrRight(Variable,3)=="sqr",
				substr(Variable, 1, nchar(Variable)-3),
				Variable
			)
		)
	# filter out NAs
	if (dim(Y[which(grepl("NA", Y$Factor2)==TRUE), ])[1] > 0) {
		Y[which(grepl("NA", Y$Factor2)==TRUE), ]$Factor2 <- NA
	}
	temp <- dcast(Y, Rotated + modelVars ~ Variable, value.var="Factor1") %>%
		mutate(Number = 1)
	Z <- dcast(Y, Rotated + modelVars ~ Variable, value.var="Factor2") %>%
		mutate(Number = 2) %>%
		merge(temp, all=T) %>%
		mutate(
			Species = factor(ifelse(
				grepl("HUMIFUS", modelVars),
				"O. humifusa",
				"O. stricta"
			)),
			Weather = factor(ifelse(
				grepl("ECIP", modelVars),
				"Precipitation",
				"Temperature"
			))
		) %>%
		merge(
			eigens[, c("Number", "Eigenvalue", "Cumulative", "modelVars")], 
			by=c("Number", "modelVars"), 
			all.x=T
		) %>%
		filter(Eigenvalue >= 1) %>%
		arrange(modelVars) %>%
		mutate(Axis = paste(dataset_type, "PC", Number)) %>%
		select(-c(modelVars, Number)) %>%
		select(Axis, everything())
	names(Z)[names(Z)=="Cumulative"] <- "Cumulative Proportion of Variance Explained"		
	names(Z)[names(Z)=="Variable"] <- "Var"		
	return(Z)
}

#' Move certain columns to the end of a dataframe
#'
#' @param data Dataset
#' @param move Variable to move? number of places to move?
#'
#' @export
movetolast <- function(data, move) {
  data[c(setdiff(names(data), move), move)]
}