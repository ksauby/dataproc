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


sas_prcomp_PCA_table_function <- function(eigenvalues, factor.pattern, dataset_type="all") {
Y <- PCA.all.surveys.eigenvalues %>%
	filter(Number < 3 & Eigenvalue >= 1) %>%		
	mutate(
		EigenNum = paste("Eigenvalue", Number, sep=""),
		PropNum = paste("Proportion", Number, sep="")	
	) %>%
	.[, c("EigenNum", "PropNum", "Eigenvalue", "Proportion", "modelVars")]
Eigens <- dcast(Y, modelVars ~ EigenNum, value.var=c("Eigenvalue"))
PropExpl <- dcast(Y, modelVars ~ PropNum, value.var=c("Proportion"))
# Factor 1
FactorVals1 <- dcast(PCA.all.surveys.factor.pattern, modelVars ~ Variable, 	value.var=c("Factor1"))
Factor1 <- 
	base::merge(
		FactorVals1,
		Eigens[, c("modelVars", "Eigenvalue1")], 
		by="modelVars"
	) %>%
	base::merge(PropExpl[, c("modelVars", "Proportion1")], by="modelVars") %>%
	mutate(
		Species = substr(modelVars,1,7),
		Weather = str_sub(modelVars, -4, -1),
		Axis = paste(dataset_type, "PC1")
		) 
# Factor 2
FactorVals2 <- dcast(PCA.all.surveys.factor.pattern, modelVars ~ Variable, 
	value.var=c("Factor2"))
Factor2 <- base::merge(
		FactorVals2,
		Eigens[, c("modelVars", "Eigenvalue2")], 
		by="modelVars"
	) %>%
	base::merge(PropExpl[, c("modelVars", "Proportion2")], by="modelVars") %>%
	mutate(
		Species = substr(modelVars,1,7),
		Weather = str_sub(modelVars, -4, -1),
		Axis = paste(dataset_type, "PC2")
		) 
		
		rbind.fill(Factor1, Factor2)
		
		
rownames(Factor1) <- Factor1[, 1]
Factor1 %<>% .[, -1] %>% t %>% as.data.frame
rownames(Factor2) <- Factor2[, 1]
Factor2 %<>% .[, -1] %>% t %>% as.data.frame
# sort into precip and temp dataframes
# sort into stricta and humifusa dataframes
grep("HUMIFUSA", names(Factor2))
Factor2[ , grep("")]
		
		
		
		
		
		
		
		
		
		
	
	(rownames=.)
	t %>%
	as.data.frame
names(Factor1) <- Factor1[1]
	
	
	melt(id.vars="modelVars")
		
		 melt(id.vars=c("modelVars", "Number"))
		rbind(c(
			"Eigenvalue", 
			PCA.all.surveys.eigenvalues$Eigenvalue[1:2] %>% round(3)
		)) %>%
		rbind(c(
			"Proportion of Variance Explained",
			summary(x)$importance[2, 1:2] %>% round(3)
		)) %>%
		data.table::setnames("PC1", paste(data.type, "PC1")) %>%
		data.table::setnames("PC2", paste(data.type, "PC2"))		
	return(y)
}