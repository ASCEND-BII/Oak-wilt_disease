################################################################################
##### Estimation of trends using Sentinel-2 time series derived from FORCE
################################################################################

#-------------------------------------------------------------------------------
# Source load

source("R/path_scenes.R")
source("R/get_mask.R")
source("R/get_trend.R")

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(terra)

#-------------------------------------------------------------------------------
#Arguments

#root_path: select the root path of where the scenes are located
#mask_months: period in which mask will be estimated for
#evaluation_months: period in which trends will be estimated
#threads: the number of threads to use for parallel processing
root_path <- "/media/antonio/Work/Oak-Sentinel/level3_sen2"
mask_doy <- c(152, 181) #The month of may
evaluation_doy <- c(152, 243) #May to August

#-------------------------------------------------------------------------------
#Function

trends_sen2 <- function(root_path, mask_doy, evaluation_doy, threads) {
  
  #Get scenes to work with
  frame <- path_scenes(root_path)
  
  #Unique tiles
  unique_tile <- unique(frame$tile)
  
  # i <- 132
  #Loop over tiles
  for(i in 1:length(unique_tile)) {
    
    #Subset to work with a given tile
    sub_tile <- subset(frame, tile == unique_tile[i])
    
    #Unique years
    unique_years <- unique(year(sub_tile$date))
    
    #ii <- 6
    #Loop over years
    for(ii in 1:length(sub_tile)) {
      
      #Subset layers for a given year
      sub_year <- subset(sub_tile, year(date) == unique_years[ii])
      sub_year$doy <- yday(sub_year$date)
      
      ###Mask-------------------------------------------------------------------
      #Mask list
      mask_scenes <- subset(sub_year, doy >= mask_doy[1] & doy <= mask_doy[2])
      mask_scenes <- paste0(root_path, "/", 
                            mask_scenes$tile, "/", mask_scenes$scene)
      
      #Mask
      mask_layer <- get_mask(mask_layer, threshold = 0.5)
      
      ###Trend------------------------------------------------------------------
      #Period of interest
      trends_scenes <- subset(sub_year, 
                       doy >= evaluation_doy[1] & doy <= evaluation_doy[2])
      
      doy <- trends_scenes$doy
      
      trends_scenes <- paste0(root_path, "/", trends_scenes$tile, 
                              "/", trends_scenes$scene)
      
      #Create a raster stack
      scenes <- rast(trends_scenes)
      
      #Apply mask
      scenes_masked <- mask(scenes,
                            mask_layer,
                            maskvalues= 0)
      #Scale back
      scenes_masked <- scenes_masked/10000
      
      #Estimate trends
      trend_layer <- app(scenes_masked, 
                         fun = fun_slope,
                         date = doy,
                         cores = threads)
      
      
    }
  }
}



