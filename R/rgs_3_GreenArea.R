# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Indicator 3.Green Area
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(maptools) 
library(rgdal)
library(rgeos)
library(jsonlite)
library(doParallel)

# using 4 cores for parallel computing
#registerDoParallel(cores=4) 

# change working directory to your own dir path where the r-geoserver.zip is unzipped to
setwd("/Users/harvey/Desktop/SDI")

# load utils methods for use
source("rgs_utils.R")

# calcuate green area index for city of melbourne using meshblock population
execIndicatorGreenArea <- function(){
  
  # the follow two lines are for testing
  boundary_url = "http://115.146.93.46:8080/geoserver/Geographic_Boundaries/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Geographic_Boundaries:qld_sa2_2011_aust&outputFormat=JSON&cql_filter=gcc_code11=%273GBRI%27"
  greenarea_url = "http://115.146.94.88:8080/geoserver/Group8/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Group8:Greenspaces%20QLD&outputFormat=json"
  populationAge_url = "http://115.146.92.210:8080/geoserver/group5_Brisbane/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=group5_Brisbane:qld_population_by_age_aus_sa2_2011_ste_gccsa_info&outputFormat=JSON&cql_filter=gcc_code11=%273GBRI%27"
  
  # load spatial object direct from geojson
  boundary = utils.loadGeoJSON2SP(boundary_url)
  # check if data layer can be successfully loaded
  if(is.null(boundary)){
    utils.debugprint("fail to load data layer for boundary in Brisbane")
    return(FALSE)
  }
  
  greenarea = utils.loadGeoJSON2SP(greenarea_url)
  # check if data layer can be successfully loaded
  if(is.null(greenarea)){
    utils.debugprint("fail to load data layer for greenarea")
    return(FALSE)
  }
  
  populationAge = utils.loadGeoJSON2DF(populationAge_url)
  # check if data layer can be successfully loaded
  if(is.null(populationAge)){
    utils.debugprint("fail to load data layer for population")
    return(FALSE)
  }
  
  # calculation
  mergedData=merge.data.frame(x=boundary@data, y=populationAge, by.x="sa2_main11", by.y="sa2_main11", sort=FALSE)
  boundary@data=mergedData
  
  boundary@data$albers_sqm <- NULL
  
  
  #pop_basenum = 100000 # green area index calculation base is for 100,000 persons
  
  # # # # # # # # # # # # # 
  # implement indicator logic here, such as
  # 1. spatial data operations like projection, intersection, union, 
  # 2. statistics generation
  # 3. etc.
  # # # # # # # # # # # # # 
  
  # project(transform) sp into UTM to enable area calculation
  sp_greenarea_prj = utils.project2UTM(greenarea)
  
  sp_boundary_prj = utils.project2UTM(boundary)
  
  # add two more attributes for sp_boundary_prj, one is for the actual size of greenarea, the other is for the greenarea index
  sp_boundary_prj@data[,"gaarea"] = 0.0
  sp_boundary_prj@data[,"idxval"] = 0.0
  
  pop_basenum = 1
  
  # ==== main loop starts here ====
  # for each population polygon, find all green areas it intersects and calcuate the size of intersected area
  # two methods are implemented:
    
  # ===================================
  # method 1 : using parallel computing
  # ===================================
  
  # bulid foreach result as a matrix containing 3 columns storing values for gaarea, idxval and mb_code11 respectively
  result = foreach(i=1:nrow(sp_boundary_prj), .combine = rbind, .export=c("SpatialPolygons","over","gIntersection","gArea")) %dopar% {

    # get the geometry polgyon of population, return 0 for gaarea and idxval if geometry is NULL
    if(is.null(sp_boundary_prj@polygons[i])){
      out = c(0, 0)
    }else{

      geom_pop = SpatialPolygons(sp_boundary_prj@polygons[i], proj4string=sp_boundary_prj@proj4string)

      # accumulate the total size of intersected greenarea for the current population geometry
      intersectedGreenArea = 0.0

      # this 'over' method is much faster to find all intersected green area polygons of current pop polygon
      # temporarily save all intersected greenarea into a sub spatialdataframe
      intersectedGADF = sp_greenarea_prj[!is.na(over(sp_greenarea_prj,sp_boundary_prj[i,]))[,1],]

      # if intersected with one or more greenarea polygon, calculate and accumulate the intersected area for each population meshblock
      if(nrow(intersectedGADF)>0){

        for(j in nrow(intersectedGADF):1){

          geom_greenarea = SpatialPolygons(intersectedGADF@polygons[j], proj4string=intersectedGADF@proj4string)

          # do the actual intersction process
          intsectedGeom = gIntersection(geom_pop, geom_greenarea)
          # accumulate the size of intersected greenarea
          intersectedGreenArea = intersectedGreenArea + gArea(intsectedGeom)

        }
      }

      # check population attribute, make sure it is valid
      population = sp_boundary_prj@data[i,"total"]

      if(is.null(population)||is.na(population)) population=0

      # for those polygons with 0 population, assign idxval = 0
      idx_val = 0
      if(population>0){
        idx_val = intersectedGreenArea / (population / (pop_basenum * 1.0))
      }

      out = c(intersectedGreenArea, idx_val)
    }
  }
  
  # assign calculated values back to sp_pop_prj@data. use as.numberic() to assure the values are numeric
  sp_boundary_prj@data[,"gaarea"] = as.numeric(result[,1])
  sp_boundary_prj@data[,"idxval"] = as.numeric(result[,2])
  
  sp_boundary_prj@data = sp_boundary_prj@data[,c("sa2_name11","total","gaarea","idxval")]
  
  
  # ===================================
  # method 2: using normal for loop
  # ===================================
  
  # this process takes long time to accomplish. 
  # in RStudio, use Ctrl+Shift+C to uncomment/comment it for testing
  
  
  # for(i in nrow(sp_pop_prj):1){
  # 
  #   utils.debugprint(sprintf("processing [%i/%i]", i, nrow(sp_pop_prj)))
  # 
  #   # get the geometry polgyon of population, skip if it is NULL
  #   if(is.null(sp_pop_prj@polygons[i])){
  #     next
  #   }
  #   geom_pop = SpatialPolygons(sp_pop_prj@polygons[i], proj4string=sp_pop_prj@proj4string)
  # 
  #   # accumulate the total size of intersected greenarea for the current population geometry
  #   intersectedGreenArea = 0.0
  # 
  #   # this 'over' method is much faster to find all intersected green area polygons of current pop polygon
  #   # temporarily save all intersected greenarea into a sub spatialdataframe
  #   intersectedGADF = sp_greenarea_prj[!is.na(over(sp_greenarea_prj,sp_pop_prj[i,]))[,1],]
  # 
  #   # if intersected with one or more greenarea polygon, calculate and accumulate the intersected area for each population meshblock
  #   if(nrow(intersectedGADF)>0){
  # 
  #     for(j in nrow(intersectedGADF):1){
  # 
  #       geom_greenarea = SpatialPolygons(intersectedGADF@polygons[j], proj4string=intersectedGADF@proj4string)
  # 
  #       # do the actual intersction process
  #       intsectedGeom = gIntersection(geom_pop, geom_greenarea)
  #       # accumulate the size of intersected greenarea
  #       intersectedGreenArea = intersectedGreenArea + gArea(intsectedGeom)
  # 
  #     }
  #   }
  # 
  #   # check population attribute, make sure it is valid
  #   population = sp_pop_prj@data[i,"persons"]
  # 
  #   if(is.null(population)||is.na(population)) population=0
  # 
  #   # for those polygons with 0 population, assign idxval = 0
  #   if(population>0){
  #     sp_pop_prj@data[i,"idxval"] = intersectedGreenArea / (population / (pop_basenum * 1.0))
  #   }
  #   # assgin intersectedGreenArea to gaarea attribute
  #   sp_pop_prj@data[i,"gaarea"] = intersectedGreenArea
  # 
  # }
  
  
  # ==== main loop ends here ====
  
  # this example shows how to publish a geolayer by creating multiple wms styles on various attributes of the same data layer. 
  # the data layer will be only published one time, with various wms styles generated for selected attributes 
  publishedinfo = utils.publishSP2GeoServerWithMultiStyles(spobj=sp_boundary_prj, 
                                                               attrname_vec=c("gaarea","idxval"),
                                                               palettename_vec=c("Reds","Blues"), 
                                                               colorreverseorder_vec=c(FALSE,FALSE), 
                                                               geomtype = "Geometry", 
                                                               colornum_vec=c(6,8), 
                                                               classifier_vec=c("Jenks","Jenks")
                                                               )
  
  if(is.null(publishedinfo) || length(publishedinfo)==0){
    utils.debugprint("fail to save data to geoserver")
    return(FALSE)
  }
  
  
  # print the outputs in json format
  utils.debugprint(sprintf("outputs: %s", toJSON(publishedinfo, auto_unbox=TRUE)))
  

  return(TRUE)
}

execIndicatorGreenArea()

