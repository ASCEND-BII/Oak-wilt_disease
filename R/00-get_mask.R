################################################################################
##### Get mask function
################################################################################

#-------------------------------------------------------------------------------
# Libraries

library(terra)

#-------------------------------------------------------------------------------
#Arguments

#' @param scenes a vector that describe the scenes with their path
#' @param threshold a threshold to separate pixels to be used (1) and non-used (0)

#-------------------------------------------------------------------------------
#Function

get_mask <- function(mask_scenes, threshold) {
 
  #load scenes
  mask_stack <- rast(mask_scenes)
  
  #apply mean function
  mask_layer <- app(mask_stack, 
                    fun = mean,
                    na.rm = TRUE)
  
  #rescale image
  mask_layer <- mask_layer/10000
  
  #kNDVI
  mask_layer <- tanh(mask_layer^2)
  
  #matrix for classification
  clasf_matix <- matrix(c(0.00, threshold, 0,
                          threshold, 1, 1),
                          ncol = 3,
                          byrow = TRUE)
  
  #mask raster
  mask_layer <- classify(mask_layer, 
                         clasf_matix, 
                         include.lowest = TRUE)
  
  return(mask_layer)
   
}
