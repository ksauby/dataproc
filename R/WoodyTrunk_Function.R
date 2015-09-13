WoodyTrunk_Function <- function(x){	
	x[
		x=="woody trunk" | 
		x=="woody tunk" | 
		x=="Woody Trunk"
	] <- "WoodyTrunk"
	return(x)
}