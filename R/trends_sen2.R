################################################################################
##### Estimation of trends using Sentinel-2 time series derived from FORCE
################################################################################

#-------------------------------------------------------------------------------
# Source load

source("R/path_scenes.R")
source("R/get_mask.R")
source("R/fun_slope.R")

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(terra)

#-------------------------------------------------------------------------------
#Arguments

#root_path: select the root path of where the scenes are located
root_path <- "/media/antonio/Work/Oak-Sentinel/level3_sen2"

#mask_months: period in which mask will be estimated for
mask_doy <- c(152, 212) #The month of june

#evaluation_months: period in which trends will be estimated
evaluation_doy <- c(166, 258) #May to August

#out_path: path of the outputs
out_path <- "/media/antonio/Work/Oak-Sentinel/level4_sen2"

#threads: the number of threads to use for parallel processing
threads <- 28


#-------------------------------------------------------------------------------
#Function
trends_sen2 <- function(root_path, mask_doy, evaluation_doy, out_path, threads) {
  
  #Get scenes to work with
  frame <- path_scenes(root_path)
  
  #Unique tiles
  unique_tile <- unique(frame$tile)
  
  #Unique years
  unique_years <- unique(year(frame$date))
  
  #Day of the year
  frame$doy <- yday(frame$date)
  
  #Progress bar
  pb <- txtProgressBar(min = 1, 
                       max = length(unique_tile)*length(unique_years), 
                       style = 3)
  
  # i <- 132
  #Loop over tiles
  for(i in 1:length(unique_tile)) {
    
    #Subset to work with a given tile
    sub_tile <- subset(frame, tile == unique_tile[i])
    
    #ii <- 6
    #Loop over years
    for(ii in 6) { #1:length(sub_tile)
      
      #Progress
      setTxtProgressBar(pb, i*ii)
      
      #Subset layers for a given year
      sub_year <- subset(sub_tile, year(date) == unique_years[ii])
      
      if(nrow(sub_year) >= 10) {
      
      ###Mask-------------------------------------------------------------------
      #Mask list
      mask_scenes <- subset(sub_year, doy >= mask_doy[1] & doy <= mask_doy[2])
      mask_scenes <- paste0(root_path, "/", 
                            mask_scenes$tile, "/", mask_scenes$scene)
      
      #Mask
      mask_layer <- get_mask(mask_scenes, threshold = 0.4)
      
      #Export mask
      name_mask <- paste0(out_path, "/", 
                          unique_tile[i], "_", 
                          unique_years[ii], "_", 
                          "mask", "_",
                          "kNDVI", ".tif")
      
      writeRaster(mask_layer, name_mask, names = "mask", overwrite=TRUE)
      
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
      names(trend_layer) <- "slope"
      
      #Export slope
      name_slope <- paste0(out_path, "/", 
                          unique_tile[i], "_", 
                          unique_years[ii], "_", 
                          "slope", "_",
                          "kNDVI", ".tif")
        
      writeRaster(trend_layer, name_slope, names = "slope", overwrite=TRUE)
      
      #Release memory -just in case-
      gc()
      
      } else{
        cat(paste0("No ", unique_tile[i], " for ", unique_years[ii]))
      }
    }
  }
}

trends_sen2(root_path, mask_doy, evaluation_doy, out_path, threads)


