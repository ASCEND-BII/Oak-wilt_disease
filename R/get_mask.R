################################################################################
##### Get mask function
################################################################################

#-------------------------------------------------------------------------------
# Libraries

library(raster)

#-------------------------------------------------------------------------------
#Arguments

#scenes: a vector that describe the scenes with their path
#threshold: a threshold to separate pixels to be used (1) and non-used (0)
#threads: select the number of threads to use for parallel computing

#-------------------------------------------------------------------------------
#Function

get_mask <- function(scenes, threshold, threads) {
 
  #Stack scenes
  raster_stack <- stack(scenes)
  
  #Start cluster
  beginCluster(threads)
  
  #layer
  mask_layer <- clusterR(x = raster_stack, 
                    fun = calc, 
                    args = list(mean, na.rm = TRUE))
  #End cluster
  endCluster()
  
  #Back scale
  mask_layer <- mask_layer/10000
  
  #Create mask
  mask_layer[mask_layer >= threshold] <- 1
  mask_layer[mask_layer < threshold] <- 0
  
  return(mask_layer)
   
}
  
  
  
  
  
}