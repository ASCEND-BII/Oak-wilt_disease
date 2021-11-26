################################################################################
##### Get mask function
################################################################################

#-------------------------------------------------------------------------------
# Libraries

library(terra)

#-------------------------------------------------------------------------------
#Arguments

#scenes: a vector that describe the scenes with their path
#threshold: a threshold to separate pixels to be used (1) and non-used (0)

#-------------------------------------------------------------------------------
#Function

get_mask <- function(scenes, threshold) {
 
  #load scenes
  mask_stack <- rast(mask_scenes)
  
  #apply mean function
  mask_raster <- app(mask_stack, 
                     fun = mean,
                     na.rm = TRUE)
  
  #rescale image
  mask_raster <- mask_raster/10000
  
  #matrix for classification
  clasf_matix <- matrix(c(0.00, threshold, 0,
                          threshold, 1, 1),
                        ncol = 3,
                        byrow = TRUE)
  
  #mask raster
  mask_layer <- classify(mask_raster, 
                         clasf_matix, 
                         include.lowest = TRUE)
  
  return(mask_layer)
   
}
