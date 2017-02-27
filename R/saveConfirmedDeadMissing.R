#' Save File with Confirmed Dead and Missing Plants
#'
#' @description write csv with dead/missing plants - remove them from the maps
#' @param Plant_Info Dataset with Plant Information
#'
#' @export
	
saveConfirmedDeadMissing <- function(Plant_Info) {
	Plant_Info_Dead <- filter(
		Plant_Info, 
		ConfirmedDead=="1" | ConfirmedMissing=="1"
	)
	Plant_Info_Dead %<>% arrange(Tag_Number, PlantID)
}