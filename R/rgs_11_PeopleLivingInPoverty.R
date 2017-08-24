# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Indicator 11.PeoplelivingInPoverty
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
execIndicatorPeoplelivingInPoverty <- function(){
  
  # the follow two lines are for testing
  boundary_url = "http://115.146.93.46:8080/geoserver/Geographic_Boundaries/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Geographic_Boundaries:qld_sa2_2011_aust&outputFormat=JSON&cql_filter=gcc_code11=%273GBRI%27"
  householdIncom_url = "http://115.146.92.210:8080/geoserver/group5_Brisbane/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=group5_Brisbane:qld_household_income_aus_sa2_2011_ste_gccsa_info&outputFOrmat=JSON&cql_filter=gcc_code11=%273GBRI%27"
  
  # load spatial object direct from geojson
  boundary = utils.loadGeoJSON2SP(boundary_url)
  # check if data layer can be successfully loaded
  if(is.null(boundary)){
    utils.debugprint("fail to load data layer for boundary in Brisbane")
    return(FALSE)
  }
  
  householdIncom = utils.loadGeoJSON2DF(householdIncom_url)
  # check if data layer can be successfully loaded
  if(is.null(householdIncom)){
    utils.debugprint("fail to load data layer for householdIncom")
    return(FALSE)
  }
  
  # calculation
  mergedData = merge.data.frame(x=boundary@data, y=householdIncom, by.x = "sa2_main11", by.y = "sa2_main11", sort = FALSE)
  boundary@data = mergedData
  
  boundary@data$albers_sqm <- NULL
  
  names(boundary@data) <- make.names(names(boundary@data))
  
  boundary@data$poverty = 0.0
  boundary@data$poverty <- with(boundary@data, (negative_income+nill_income+X1_10400+X10400_15599+X15600_20799+X20800_31199)/total)
  
  boundary@data = boundary@data[,c("sa2_name11","sa3_name11","sa4_name11","gcc_name11.x","ste_name11.x","negative_income","nill_income","X1_10400","X10400_15599","X15600_20799","X20800_31199","poverty")]
  
  
  
  # this example shows how to publish a geolayer by creating multiple wms styles on various attributes of the same data layer. 
  # the data layer will be only published one time, with various wms styles generated for selected attributes 
  publishedinfo = utils.publishSP2GeoServerWithMultiStyles(spobj=boundary, 
                                                               attrname_vec=c("poverty"),
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

execIndicatorPeoplelivingInPoverty()

