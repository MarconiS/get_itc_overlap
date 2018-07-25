
clip_image_with_data <- function(polygon_id, shp, wd, site_id, rgb){
  tryCatch({
    library(rgdal)
    library(raster)
    ras_pt = list.files(path = paste(wd, "itc_tiff", site_id, sep = "/"), pattern = paste(polygon_id, "tif", sep="."))
    ground <- shp[shp@data$ID == polygon_id, ]

    for(pt in ras_pt){
      ras_dat <- stack(paste(wd, "itc_tiff", site_id, ras_pt, sep = "/"))
      crs(ras_dat) <- CRS(paste("+init=epsg:", epsg, sep=""))
      rgb <- stack("~/Documents/Macrosystem/osbs_noval_ortho.tif")
      rgb <- crop(rgb, extent(ras_dat))
      pred <- readOGR(paste(wd, "itc_polygons",site_id,  sep = "/"), gsub('.{4}$', '', pt))
      #rgb file
      png(filename=paste(wd, "/images/",gsub('.{4}$', '', pt), 'ortho.png', sep = ""))
      plotRGB(rgb, 1,2,3)
      plot(pred, add = T, border = "magenta", lwd=4)
      plot(ground, add = T, border = "orange", lwd=4)
      dev.off()
      #hyperspectral and chm
      png(filename=paste(wd, "/images/",gsub('.{4}$', '', pt), 'hps_chm.png', sep = ""))
      plotRGB(ras_dat, 17, 81, 135, scale = 15000, stretch="hist")
      plot(ras_dat[[1]], add = T, col = gray.colors(10, start = 0.1, end = 1, gamma = 1.4, alpha = 0.3), legend = F)
      plot(pred, add = T, border = "magenta", lwd=4)
      plot(ground, add = T, border = "orange", lwd=4)
      dev.off()
      #chm
      png(filename=paste(wd, "/images/", gsub('.{4}$', '', pt), 'chm.png', sep = ""))
      plot(ras_dat[[1]], col = gray.colors(10, start = 0.1, end = 1, gamma = 1.4, alpha = NULL), legend = F)
      plot(pred, add = T, border = "magenta", lwd=4)
      plot(ground, add = T, border = "orange", lwd=4)
      dev.off()
    }
  },error=function(e){})
}

library(rgdal)
wd = "/Users/sergiomarconi/Documents/Chapter1/AOP_Retriever/outputs_comp/"
site_id = "OSBS"
epsg = 32617
shp <- readOGR(paste(wd, "data", sep = "/"), "competitions_centroids")
colnames(shp@data)[1] <- "ID"
ids <- shp@data$ID
lapply(ids, clip_image_with_data, shp, wd, site_id, rgb)

