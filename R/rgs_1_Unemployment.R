# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Indicator 1.Unemployment
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
execIndicatorUnemployment <- function(){
  
  # the follow two lines are for testing
  boundary_url = "http://115.146.93.46:8080/geoserver/Geographic_Boundaries/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Geographic_Boundaries:qld_sa2_2011_aust&outputFormat=JSON&cql_filter=gcc_code11=%273GBRI%27"
  employmentSex_url = "http://115.146.92.236:8080/geoserver/group2/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=group2:qld_employment_by_sex_and_hours_aus_sa2_2011_ste_gccsa_info&outputFormat=JSON&cql_filter=gcc_code11=%273GBRI%27"
  
  # load spatial object direct from geojson
  boundary = utils.loadGeoJSON2SP(boundary_url)
  # check if data layer can be successfully loaded
  if(is.null(boundary)){
    utils.debugprint("fail to load data layer for boundary in Brisbane")
    return(FALSE)
  }
  
  employment = utils.loadGeoJSON2DF(employmentSex_url)
  # check if data layer can be successfully loaded
  if(is.null(employment)){
    utils.debugprint("fail to load data layer for employment")
    return(FALSE)
  }
  
  # calculation
  mergedData = merge.data.frame(x=boundary@data, y=employment, by.x = "sa2_main11", by.y = "sa2_main11", sort = FALSE)
  boundary@data = mergedData
  
  boundary@data$albers_sqm <- NULL
  
  #delete useless attributes
  
  
  boundary@data$unemp = 0.0
  boundary@data$unemp <- with(boundary@data, unemployed_looking_for_work_persons/total) 
  
  boundary@data = boundary@data[,c("sa2_name11","sa3_name11","sa4_name11","gcc_name11.x","ste_name11.x","unemployed_looking_for_work_persons","total","unemp")]
  
  # this example shows how to publish a geolayer by creating multiple wms styles on various attributes of the same data layer. 
  # the data layer will be only published one time, with various wms styles generated for selected attributes 
  publishedinfo = utils.publishSP2GeoServerWithMultiStyles(spobj=boundary, 
                                                               attrname_vec=c("unemp"),
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

execIndicatorUnemployment()

