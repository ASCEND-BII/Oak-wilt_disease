################################################################################
##### Mosaic of layers with different extent
################################################################################

#-------------------------------------------------------------------------------
# Libraries

library(terra)

#-------------------------------------------------------------------------------
#Arguments

#path: local folder where the scenes are located
path <- "/media/antonio/Work/Oak-Sentinel/level4_sen2"

#pattern: pattern name of the scenes
pattern <- "slope_kNDVI.tif"

#-------------------------------------------------------------------------------
# Function
get_mosaic <- function(path, pattern) {
  
  #Get files
  files <- list.files(path = path, 
                      pattern = pattern, 
                      all.files = TRUE,
                      full.names = TRUE, 
                      recursive = TRUE)
  
  cat("A total of", length(files), "are been merge")
  
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
  
  return(mosaic_layer)
  
}