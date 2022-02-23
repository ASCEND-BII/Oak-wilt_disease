################################################################################
##### Mosaic of layers with different extent
################################################################################

#-------------------------------------------------------------------------------
# Libraries

library(terra)

#-------------------------------------------------------------------------------
#Arguments

#' @param path local folder where the scenes are located
#' @param pattern pattern name of the scenes
#' @param out_path: path of the outputs including the name, if not return a raster

path <- "/media/antonio/Work/Oak-wilt/level4_sen2"
pattern <- "mask.tif"
out_path <- "/media/antonio/Work/Oak-wilt/level4_sen2/2021_mask.tif"

#-------------------------------------------------------------------------------
# Function
get_mosaic <- function(path, pattern, output = NULL) {
  
  #Get files
  files <- list.files(path = path, 
                      pattern = pattern, 
                      all.files = TRUE,
                      full.names = TRUE, 
                      recursive = TRUE)
  
  cat("A total of", length(files), "images will be merged")
  
  #Create a list of raster
  raster_list <- list()
  
  for(i in 1:length(files)) {
    raster_layer <- rast(files[i])
    raster_list[[i]] <- raster_layer
  }
  
  #Create raster collection
  raster_collection <- src(raster_list)
  
  #Create mosaic
  mosaic_layer <- mosaic(raster_collection)
  
  if(!is.NULL(output)) {
    writeRaster(mosaic_layer, out_path, names = "mask", overwrite=TRUE)
  }
  
  return(mosaic_layer)
  
}