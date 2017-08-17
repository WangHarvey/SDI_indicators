# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Indicator 4.JobPerHousingRatio
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(maptools) 
library(rgdal)
library(rgeos)
library(jsonlite)
library(doParallel)

# using 1 cores for computing
# registerDoParallel(cores=1) 

# change working directory to your own dir path where the r-geoserver.zip is unzipped to
setwd("/Users/harvey/Desktop/SDI")

# load utils methods for use
source("rgs_utils.R")

# calcuate unemployment indicator for city of brisbane
execIndicatorJobPerHousingRatio <- function(){
  
  # the follow two lines are for testing
  boundary_url = "http://115.146.93.46:8080/geoserver/Geographic_Boundaries/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Geographic_Boundaries:qld_sa2_2011_aust&outputFormat=JSON&cql_filter=gcc_code11=%273GBRI%27"
  dwellings_url = "http://115.146.94.42:8080/geoserver/Group_1/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Group_1:QLD_Dwellings_HomelessShelters&outputFormat=JSON&cql_filter=gcc_code11=%273GBRI%27"
  employmentSector_url = "http://115.146.92.236:8080/geoserver/group2/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=group2:qld_employment_by_sector_aus_sa2_2011_ste_gccsa_info&outputFormat=json&cql_filter=gcc_code11=%273GBRI%27"
  
  # load spatial object direct from geojson
  boundary = utils.loadGeoJSON2SP(boundary_url)
  # check if data layer can be successfully loaded
  if(is.null(boundary)){
    utils.debugprint("fail to load data layer for boundary in Brisbane")
    return(FALSE)
  }
  
  dwellings = utils.loadGeoJSON2DF(dwellings_url)
  # check if data layer can be successfully loaded
  if(is.null(dwellings)){
    utils.debugprint("fail to load data layer for dwelling")
    return(FALSE)
  }
  
  employmentSector = utils.loadGeoJSON2DF(employmentSector_url)
  # check if data layer can be successfully loaded
  if(is.null(employmentSector)){
    utils.debugprint("fail to load data layer for employmentCategory")
    return(FALSE)
  }
  
  # calculation
  mergedData=merge.data.frame(x=boundary@data, y=employmentSector, by.x="sa2_code11", by.y="sa2_code11", sort=FALSE)
  mergedDataAll=merge.data.frame(x=mergedData, y=dwellings, by.x="sa2_code11", by.y="sa2_code11", sort=FALSE)
  boundary@data=mergedDataAll
  
  boundary@data$albers_sqm <- NULL
  
  boundary@data$jobPerHousingRatio = 0.0
  boundary@data$jobPerHousingRatio = with(boundary@data,total.x/total.y)
  
  # this example shows how to publish a geolayer by creating multiple wms styles on various attributes of the same data layer. 
  # the data layer will be only published one time, with various wms styles generated for selected attributes 
  publishedinfo = utils.publishSP2GeoServerWithMultiStyles(spobj=boundary, 
                                                               attrname_vec=c("JobPerHousingRatio"),
                                                               palettename_vec=c("Blues"), 
                                                               colorreverseorder_vec=c(FALSE), 
                                                               geomtype = "Geometry", 
                                                               colornum_vec=c(8), 
                                                               classifier_vec=c("Jenks")
                                                               )
  
  if(is.null(publishedinfo) || length(publishedinfo)==0){
    utils.debugprint("fail to save data to geoserver")
    return(FALSE)
  }
  
  
  # print the outputs in json format
  utils.debugprint(sprintf("outputs: %s", toJSON(publishedinfo, auto_unbox=TRUE)))
  

  return(TRUE)
}

execIndicatorJobPerHousingRatio()

