#' Convert the format of multiple columns
#' @param dataset
#' @param proj4stringdata
#' @param CRSdata
#' @param NameField
#' @param filename
#' @references # inspired by the answer from rcs on http://stackoverflow.com/questions/7813141/how-to-create-a-kml-file-using-r
#' @examples
#' Plot_Info_Islands = read.csv(file = "../../../Written-Documents/Data-forms/island-data-collection-form/IslandRoadway1_MasterPlotList_OrderedbyGeography_Unsurveyed.csv", header = TRUE, stringsAsFactors = FALSE)
#' # Roadway 1
#' CreateKML_Function(dataset=Unsurveyed.Plots, proj4stringdata="+proj=utm +zone=17 +datum=WGS84", CRSdata="+proj=longlat +datum=WGS84", NameField="Tag_Number", filename="Unsurveyed.Plots")
#' # EXAMPLE
#' # create KML of unsurveyed plots - dataset "Unsurveyed"
#' # The points should be named based on the column "Tag_Number"
#' # give the KML file the name "unsurveyed_7sept14"
#' CreateKML(dataset=Unsurveyed, proj4stringdata="+proj=utm +zone=17 +datum=WGS84",
#'	CRSdata="+proj=longlat +datum=WGS84", NameField="Tag_Number", 
#'	filename="unsurveyed_7sept14")
#' @export

CreateKML_Function <- function(
	dataset=dataset, 
	proj4stringdata=proj4stringdata, 
	CRSdata=CRSdata, 
	NameField=NameField, 
	filename=filename
) 
{
	dataset$Easting <- as.numeric(dataset$Easting)
	dataset$Northing <- as.numeric(dataset$Northing)
	coordinates(dataset) <- ~ Easting + Northing
	proj4string(dataset) <- proj4stringdata
	dataset_transform <- spTransform(dataset, CRS(CRSdata))
	writeOGR(dataset_transform, paste(filename, ".kml",sep=""), 
	layer="NewOrder", dataset_options=paste("NameField=",NameField,sep=""), driver="KML")
}

