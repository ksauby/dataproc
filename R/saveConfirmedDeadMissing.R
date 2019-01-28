#' Save File with Confirmed Dead and Missing Plants
#'
#' @description write csv with dead/missing plants - remove them from the maps
#' @param Plant_Info Dataset with Plant Information
#'
#' @export
	
saveConfirmedDeadMissing <- function(Plant_Info) {
	Plant_Info_Dead <- Plant_Info %>% filter( 
		.data$ConfirmedDead=="1" | .data$ConfirmedMissing=="1"
	)
	Plant_Info_Dead %<>% arrange(.data$Tag_Number, .data$PlantID)
}