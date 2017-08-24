# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Indicator 7.HomelessShelters
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
execIndicatorHomelessShelters <- function(){
  
  # the follow two lines are for testing
  boundary_url = "http://115.146.93.46:8080/geoserver/Geographic_Boundaries/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Geographic_Boundaries:qld_sa2_2011_aust&outputFormat=JSON&cql_filter=gcc_code11=%273GBRI%27"
  dwellings_url = "http://115.146.94.42:8080/geoserver/Group_1/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Group_1:QLD_Dwellings_HomelessShelters&outputFormat=JSON&cql_filter=gcc_code11=%273GBRI%27"
  homeless_url = "http://115.146.94.44:8080/geoserver/Group_4/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Group_4:homeless_count_aus_sa2_2011_ste_gccsa_info&outputFormat=JSON&cql_filter=gcc_code11=%273GBRI%27"
  
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
  
  homeless = utils.loadGeoJSON2DF(homeless_url)
  # check if data layer can be successfully loaded
  if(is.null(homeless)){
    utils.debugprint("fail to load data layer for homeless")
    return(FALSE)
  }
  
  # calculation
  mergedData=merge.data.frame(x=boundary@data, y=dwellings, by.x="sa2_code11", by.y="sa2_code11", sort=FALSE, all.x = TRUE)
  mergedDataAll=merge.data.frame(x=mergedData, y=homeless, by.x="sa2_main11", by.y="sa2_main11", sort=FALSE, all.x = TRUE)
  boundary@data=mergedDataAll
  
  boundary@data$albers_sqm <- NULL
  
  boundary@data$homesh = 0.0
  boundary@data$homesh = with(boundary@data,hostel_for_homeless_and_night_shelter/homeless_count)
  
  boundary@data = boundary@data[,c("sa2_name11","sa3_name11","sa4_name11","gcc_name11.x","ste_name11.x","hostel_for_homeless_and_night_shelter","homeless_count","homesh")]
  
  
  # this example shows how to publish a geolayer by creating multiple wms styles on various attributes of the same data layer. 
  # the data layer will be only published one time, with various wms styles generated for selected attributes 
  publishedinfo = utils.publishSP2GeoServerWithMultiStyles(spobj=boundary, 
                                                               attrname_vec=c("homesh"),
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

execIndicatorHomelessShelters()

