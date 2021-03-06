#' Create a KML from a dataframe
#' 
#' @param dataset Dataset
#' @param proj4stringdata The projection data for the dataset.
#' @param CRSdata The projection to which the dataset should be converted.
#' @param NameField The attribute to use to name the points.
#' @param filename Name of the KML file to be generated.
#' @description Create a KML Google Earth file from a dataframe.
#' @references # inspired by the answer from rcs on http://stackoverflow.com/questions/7813141/how-to-create-a-kml-file-using-r
#' @examples
#' Plot_Info_Islands = read.csv(file = "../../../Written-Documents/Data-forms/island-data-collection-form/IslandRoadway1_MasterPlotList_OrderedbyGeography_Unsurveyed.csv", header = TRUE, stringsAsFactors = FALSE)
#' # Roadway 1
#' CreateKML_Function(dataset=Florida.ghcnd.stations, proj4stringdata="+proj=utm +zone=17 +datum=WGS84", CRSdata="+proj=longlat +datum=WGS84", NameField="Tag_Number", filename="Florida.ghcnd.stations")
#' # EXAMPLE
#' # create KML of unsurveyed plots - dataset "Unsurveyed"
#' # The points should be named based on the column "Tag_Number"
#' # give the KML file the name "unsurveyed_7sept14"
#' CreateKML(
#'	dataset=Unsurveyed, 
#'	proj4stringdata="+proj=utm +zone=17 +datum=WGS84",
#'	CRSdata="+proj=longlat +datum=WGS84", 
#'	NameField="Tag_Number", 
#'	filename="unsurveyed_7sept14"
#')
#' 
#' @export
#' @importFrom sp coordinates spTransform CRS proj4string
#' @importFrom rgdal writeOGR

CreateKML_Function <- function(
	dataset=dataset, 
	proj4stringdata=proj4stringdata, 
	CRSdata=CRSdata, 
	NameField=NameField, 
	filename=filename
) {
	dataset$Easting <- as.numeric(dataset$Easting)
	dataset$Northing <- as.numeric(dataset$Northing)
	coordinates(dataset) <- ~ Easting + Northing
	proj4string(dataset) <- proj4stringdata
	dataset_transform <- spTransform(dataset, CRS(CRSdata))
	writeOGR(dataset_transform, paste(filename, ".kml",sep=""), 
	layer="NewOrder", dataset_options=paste("NameField=",NameField,sep=""), driver="KML")
}

