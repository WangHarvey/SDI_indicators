# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Indicator 2.JobAvailability
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
execIndicatorJobAvailability <- function(){
  
  # the follow two lines are for testing
  boundary_url = "http://115.146.93.46:8080/geoserver/Geographic_Boundaries/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Geographic_Boundaries:qld_sa2_2011_aust&outputFormat=JSON&cql_filter=gcc_code11=%273GBRI%27"
  #employmentSector_url = "http://115.146.92.236:8080/geoserver/group2/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=group2:qld_employment_by_sector_aus_sa2_2011_ste_gccsa_info&outputFormat=json&cql_filter=gcc_code11=%273GBRI%27"
  employmentCategory_url = "http://115.146.92.236:8080/geoserver/group2/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=group2:qld_employment_categories_aus_sa2_2011_ste_gccsa_info&outputFormat=json&cql_filter=gcc_code11=%273GBRI%27"
  
  # load spatial object direct from geojson
  boundary = utils.loadGeoJSON2SP(boundary_url)
  # check if data layer can be successfully loaded
  if(is.null(boundary)){
    utils.debugprint("fail to load data layer for boundary in Brisbane")
    return(FALSE)
  }
  
  #employmentSector = utils.loadGeoJSON2DF(employmentSector_url)
  # check if data layer can be successfully loaded
 # if(is.null(employmentSector)){
  #  utils.debugprint("fail to load data layer for employment(sector)")
  #  return(FALSE)
  #}
  
  employmentCategory = utils.loadGeoJSON2DF(employmentCategory_url)
  # check if data layer can be successfully loaded
  if(is.null(employmentCategory)){
    utils.debugprint("fail to load data layer for employment(category)")
    return(FALSE)
  }
  
  # calculation
  mergedData = merge.data.frame(x=boundary@data, y=employmentCategory, by.x = "sa2_main11", by.y = "sa2_main11", sort = FALSE)
  #mergedDataAll = merge.data.frame(x=mergedData, y=employmentCategory, by.x = "sa2_main11", by.y = "sa2_main11", sort = FALSE)
  boundary@data = mergedData
  
  boundary@data$albers_sqm <- NULL
  
  boundary@data$job_av = 0.0
  boundary@data$job_av <- with(boundary@data, (employed_full_time+employed_part_time+employed_away_from_work+employed_hours_of_work_not_stated)/total)
  
  boundary@data = boundary@data[,c("sa2_name11","sa3_name11","sa4_name11","gcc_name11.x","ste_name11.x","employed_full_time","employed_part_time","employed_away_from_work","employed_hours_of_work_not_stated","total","job_av")]
  
  # this example shows how to publish a geolayer by creating multiple wms styles on various attributes of the same data layer. 
  # the data layer will be only published one time, with various wms styles generated for selected attributes 
  publishedinfo = utils.publishSP2GeoServerWithMultiStyles(spobj=boundary, 
                                                               attrname_vec=c("job_av"),
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

execIndicatorJobAvailability()

