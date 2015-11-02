#' Build a table of PCA results.
#' @param x output of the \code{prcomp} function.
#' Label to append to PC 1 and PC 2 column headers; particularly useful if one wants to display data from multiple PCAs.

[51] "PCA.all.surveys.eigenvalues"                                                          
[52] "PCA.all.surveys.factor.pattern"                                                       
[53] "PCA.all.surveys.rotated.factor.pattern"                                               
[54] "PCA.spring.eigenvalues"                                                               
[55] "PCA.spring.factor.pattern"                                                            
[56] "PCA.spring.rotated.factor.pattern"                                                    
[57] "PCA.winter.eigenvalues"                                                               
[58] "PCA.winter.factor.pattern"                                                            
[59] "PCA.winter.rotated.factor.pattern"                                                    


X <- PCA.all.surveys.eigenvalues %>%
	filter(Number < 3 & Eigenvalue >= 1) %>%		
	mutate(
		EigenNum = paste("Eigen", Number, sep=""),
		PropNum = paste("Prop", Number, sep="")	
	) %>%
	.[, c("EigenNum", "PropNum", "Eigenvalue", "Proportion", "modelVars")]
Eigens <- dcast(X, modelVars ~ EigenNum, value.var=c("Eigenvalue"))
PropExpl <- dcast(X, modelVars ~ PropNum, value.var=c("Proportion"))
# process data for each factor
Y <- list()
for (i in 1:n.axes) {
	FactorVals <- dcast(
		PCA.all.surveys.factor.pattern, 
		modelVars ~ Variable, 
		value.var = paste("Factor", i, sep="")
	)
	Y[[i]] <- 
		merge(
			FactorVals,
			Eigens[, c("modelVars", paste("Eigen", i, sep=""))], 
			by="modelVars"
		) %>%
		merge(
			PropExpl[, c("modelVars", paste("Prop", i, sep=""))], 
			by="modelVars"
		) %>%
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
			)),
			Axis = factor(paste(dataset_type, paste("PC", i, sep="")))
		) 
	names(Y[[i]])[names(Y[[i]])==paste("Eigen", i, sep="")] <- "Eigenvalue"		
	names(Y[[i]])[names(Y[[i]])==paste("Prop", i, sep="")] <- "Proportion"		
}
# merge Factor 1 and Factor 2 dataframes
Z <- rbind.fill(Y)
# which columns are numeric?		
nums <- sapply(Z, is.numeric)
Z[, nums] %<>% round(round_n)
# filter out Axes that had Eigenvalues less than 1
Z %<>% filter(!is.na(Eigenvalue))
for (i in 1:dim(Z)[2]) {
	if (substr(names(Z)[i],1,3)=="log") {
		Z[, i] <- paste(Z[, i], "(log)")
		names(Z)[i] <- substring(names(Z)[i], 4)
	}
	if (substr(names(Z)[i],1,4)=="sqrt") {
		Z[, i] <- paste(Z[, i], "(sqrt)")
		names(Z)[i] <- substring(names(Z)[i], 5)
	}
}
Z %<>% 
	melt(id.vars=c("modelVars", "Species", "Weather", "Axis")) %>%
	filter(!is.na(value)) %>%		
	.[which(grepl("NA", .$value)==FALSE), ] %>%
	as.data.table %>%
	unique %>%
	reshape2::dcast(modelVars+Species+Weather+Axis ~ variable, value.var="value") %>% 
	filter(Species=="O. humifusa" & Weather=="Precipitation")

	# why did some values disappear?