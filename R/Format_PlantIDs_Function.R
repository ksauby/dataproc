#' Format PlantID
#' 
#' @param x Vectors of PlotPlantID data.
#' @description Plants that spread into multiple plots have subscripts so that each PlotPlantID is associated with only one plot. This function renames Plant IDs unique to each plot (relevant only for plants found in multiple plots).
#' 
#' @export

Format_PlantIDs_Function <- function(x){
	x$PlotPlantID <- x$PlantID
	# remove 5th digit from plant ID
	x$PlantID %<>% substr(1,4) 
	return(x)
}