sas_prcomp_PCA_table_function <- function(eigenvalue.data, factor.pattern.data, dataset_type="all", round_n=3, n.axes=2) {
	# for rotated factor pattern data, check that there are two sets of data for stricta and two sets for humifusa
	
	# for those not in the rotated factor pattern data
	
	
	nums <- sapply(factor.pattern.data, is.numeric)
	factor.pattern.data[, nums] %<>% round(round_n)
	X <- factor.pattern.data %>% 
		# edit cells so they reflect transformations
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
		if (dim(X[which(grepl("NA", X$Factor2)==TRUE), ])[1] > 0) {
			X[which(grepl("NA", X$Factor2)==TRUE), ]$Factor2 <- NA
		}
	temp <- dcast(X, modelVars ~ Variable, value.var="Factor1") %>%
		mutate(Number = 1)
	Y <- dcast(X, modelVars ~ Variable, value.var="Factor2") %>%
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
		merge(eigenvalue.data[, c("Number", "Eigenvalue", "Proportion", "modelVars")], by=c("Number", "modelVars"), all.x=T) %>%
		filter(Eigenvalue >= 1)
	return(Y)
}