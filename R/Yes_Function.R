Yes_Function <- function(x){	
	x[which(
		x=="Yes" | 
		x=="yes" |
		x=="y" |
		x=="Egg" | 
		x=="Larva" | 
		x=="Nymph"
	)] <- "1"
	return(x)
}