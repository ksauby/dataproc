sas_prcomp_PCA_table_function <- function(eigens, factors, rfactors, dataset_type="all", round_n=3, n.axes=2) {
	# which models are NOT in the rotated factor pattern data? merge them with the rotated factor pattern data
	factors %<>% 
		filter(!(modelVars %in% rfactors$modelVars)) %>%
		mutate(Rotated = "No")
	if (dim(factors)[1] > 0) {
		X <- rfactors %>%
			mutate(Rotated = "Yes") %>%
			rbind.fill(factors)
	} else {
		X <- rfactors %>%
			mutate(Rotated = "Yes")
	}	
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
				substr(modelVars,1,7) == "HUMIFUS",
				"O. humifusa",
				"O. stricta"
			)),
			Weather = factor(ifelse(
				str_sub(modelVars, -4, -1) == "ECIP",
				"Precipitation",
				"Temperature"
			))
		) %>%
		merge(
			eigens[, c("Number", "Eigenvalue", "Proportion", "modelVars")], 
			by=c("Number", "modelVars"), 
			all.x=T
		) %>%
		filter(Eigenvalue >= 1) %>%
		arrange(modelVars) %>%
		mutate(Axis = paste(dataset_type, "PC", Number)) %>%
		dplyr::select(-c(modelVars, Number)) %>%
		dplyr::select_(.dots=c("Axis", "A1", "A2", "B", "C1", "C2", "C3", "D1", 
			"D2", "D3", "E1", "E2", "F", "G", "H1", "H2", "Eigenvalue", 
			"Proportion", "Rotated", "Species", "Weather"))
	names(Z)[names(Z)=="Proportion"] <- "Proportion of Variance Explained"		
	names(Z)[names(Z)=="Variable"] <- "Var"		
	return(Z)
}