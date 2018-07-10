get_jaccard <- function(pred_itc_pt=NULL, ground_pt=NULL, dataname=NULL, epsg = NULL){
  
  library(rgdal)
  library(dplyr)
  library(raster)
  library(doParallel)
  library(rlas)
  library(rgeos)
  library(clue)
  library(raster)
  library(lidR)
  source("./src/functions.R")

  #### READ IN ground data
  itcs <- readOGR(dsn=ground_pt, layer = dataname,stringsAsFactors = F)
  
  #create dataframe
  itc_df <- itcs@data
  itc_df$Id <- as.integer(itc_df$Id)
  # how many crowns in total? 628
  
  listPlot <- list.files("~/Documents/ITCs/TALL_polygons/ITCs/", pattern = ".shp")
  listPlot <- substr(listPlot,1,nchar(listPlot)-4) 
  
  #loop in each prediction polygon
  jaccard_stats = list()
  for(i in listPlot){
    tryCatch({ 
      print(i)
      pred_itc <- readOGR(dsn=pred_itc_pt, layer = i,stringsAsFactors = F, verbose = F)
      proj4string(pred_itc) <-  CRS(paste("+init=epsg:", epsg, sep=""))
      itcs <-spTransform(itcs, CRS(paste("+init=epsg:", epsg, sep="")))
      if(!is.null(crop(pred_itc, extent(itcs)))){
        print("gotcha!")
        jac.dalponte[[i]] <- overJac(itcs, pred_itc)
      }
    }, error=function(e){cat(paste("ERROR in ", i, " :" ,sep="~"),conditionMessage(e), "\n")})
  }
}

#debug paths
pred_itc_pt = "~/Documents/ITCs/TALL_polygons/ITCs/"
ground_pt = "~/Documents/ITCs/TALL_crown_polygons/"
dataname = "TALL_sample_crowns_edits_May2018"
epsg <- 32616

get_jaccard(pred_itc_pt, ground_pt, dataname, epsg)
