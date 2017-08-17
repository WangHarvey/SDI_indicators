# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Indicator 9.TeacherStudentRatio
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
execIndicatorTeacherStudentRatio <- function(){
  
  # the follow two lines are for testing
  boundary_url = "http://115.146.93.46:8080/geoserver/Geographic_Boundaries/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Geographic_Boundaries:qld_sa2_2011_aust&outputFormat=JSON&cql_filter=gcc_code11=%273GBRI%27"
  teacher_url = "http://115.146.93.63:8080/geoserver/group_9/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=group_9:Teachers_Pop_QLD&outputFormat=json&cql_filter=gcc_code11=%273GBRI%27"
  student_url = "http://115.146.93.63:8080/geoserver/group_9/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=group_9:Student_Pop_QLD&outputFormat=json&cql_filter=gcc_code11=%273GBRI%27"
  
  
  # load spatial object direct from geojson
  boundary = utils.loadGeoJSON2SP(boundary_url)
  # check if data layer can be successfully loaded
  if(is.null(boundary)){
    utils.debugprint("fail to load data layer for boundary in Brisbane")
    return(FALSE)
  }
  
  teacher = utils.loadGeoJSON2DF(teacher_url)
  # check if data layer can be successfully loaded
  if(is.null(teacher)){
    utils.debugprint("fail to load data layer for teacher")
    return(FALSE)
  }
  
  student = utils.loadGeoJSON2DF(student_url)
  # check if data layer can be successfully loaded
  if(is.null(student)){
    utils.debugprint("fail to load data layer for student")
    return(FALSE)
  }
  
  # calculation
  mergedData=merge.data.frame(x=boundary@data, y=teacher, by.x="sa2_main11", by.y="sa2_main11", sort=FALSE, all.x = TRUE)
  mergedDataAll=merge.data.frame(x=mergedData, y=student, by.x="sa2_main11", by.y="sa2_main11", sort=FALSE, all.x = TRUE)
  boundary@data=mergedDataAll
  
  boundary@data$albers_sqm <- NULL
  
  boundary@data$teacherStudentRatio = 0.0
  boundary@data$teacherStudentRatio = with(boundary@data,(primary_school_teachers+secondary_school_teachers+middle_school_teachers)/full_time_student)
  
  
  # this example shows how to publish a geolayer by creating multiple wms styles on various attributes of the same data layer. 
  # the data layer will be only published one time, with various wms styles generated for selected attributes 
  publishedinfo = utils.publishSP2GeoServerWithMultiStyles(spobj=boundary, 
                                                               attrname_vec=c("TeacherStudentRatio"),
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

execIndicatorTeacherStudentRatio()

