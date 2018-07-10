assignJaccard <- function(bOverlap, itc_id){
  out <- rep(0, length(itc_id))
  for(ii in 1:length(itc_id)){
    if(itc_id[ii] != 0){
      out[ii] <- bOverlap[ii, itc_id[ii]]
    }
  }
  return(out)
}

jaccard_summary <- function(dat){
  prod <- unlist(dat) %>%
    lapply( function(x) {x[x!=0]}) 
  return(summary(unlist(prod), na.rm = T))
}

overJac <- function(itc, itcSeg){
  library(rgeos)
  tryCatch({ 
    itcSeg <- crop(itcSeg, extent(itc))
    #check if the polygon is empty: put a warning and leave the function
    bOverlap <- matrix(NA, ncol=length(itcSeg@data), nrow=length(itc@data$crown_id))
    if(!is.null(itcSeg)){
      for(iii in 1:dim(bOverlap)[1]){
        foo <- itc[iii,]
        for(mmm in 1:dim(bOverlap)[2]){
          itcIntersection = gIntersection(foo, itcSeg[mmm,])
          if(is.null(itcIntersection)){
            bOverlap[iii, mmm] <- 0
          }else{
            bOverlap[iii, mmm] <-gArea(itcIntersection)/
              (gArea(foo) + gArea(itcSeg[mmm,]) - gArea(itcIntersection))
          }
        }
      }
    }else{
      overITC <- 0
    }  
    # here is where the Hungarian algorithm should go
    FalseNeg <- dim(bOverlap)[1] - dim(bOverlap)[2]
    if(FalseNeg > 0){
      assigned = solve_LSAP(t(bOverlap), maximum = T)
      aveOve <- assignJaccard(t(bOverlap), assigned) 
      aveOve <- c(aveOve, rep(0,FalseNeg ))
    }else{
      assigned = solve_LSAP((bOverlap), maximum = T)
      aveOve <- assignJaccard((bOverlap), assigned) 
    }
  }, error=function(e){cat(paste("ERROR in ", itc, " :" ,sep="~"),conditionMessage(e), "\n")})
  return(aveOve)
}


## Define the function
gdal_polygonizeR <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile',
                             pypath=NULL, readpoly=TRUE, quiet=TRUE) {
  if (isTRUE(readpoly)) require(rgdal)
  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')
  }
  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists))
      stop(sprintf('File already exists: %s',
                   toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.tif')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  system2('python3', args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
                                   pypath, rastpath, gdalformat, outshape)))
  if (isTRUE(readpoly)) {
    shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
    return(shp)
  }
  return(NULL)
}
