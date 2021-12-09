################################################################################
##### Estimation of trends using Sentinel-2 time series derived from FORCE
################################################################################

#-------------------------------------------------------------------------------
# Source load

source("R/00-path_scenes.R")
source("R/00-fun_slope.R")

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(terra)

#-------------------------------------------------------------------------------
#Arguments

#' @param root_path: select the root path of where the scenes are located
#' @param evaluation_doy: period in which trends will be estimated
#' @param band_select: select the band of interest
#' @param mask_path the path where the mask files are located
#' @param out_path: path of the outputs
#' @param overwrite if the file exist, do you want to create it again?
#' @param threads: the number of threads to use for parallel processing

root_path <- "/media/antonio/Work/Oak-wilt/level3_sen3"
evaluation_doy <- c(166, 258) #May to August
band_select <- "KNV"
mask_path <- "/media/antonio/Work/Oak-wilt/level4_mask" 
out_path <- "/media/antonio/Work/Oak-wilt/level4_sen2"
threads <- 32

#-------------------------------------------------------------------------------
#Function
trends_sen2 <- function(root_path, evaluation_doy, band_select, mask_path, out_path, overwrite = FALSE, threads) {
  
  #Get scenes to work with
  frame <- path_scenes(root_path)
  
  #Subset
  frame <- frame[band == band_select]
  
  #Unique tiles
  unique_tile <- unique(frame$tile)
  
  #Unique years
  unique_years <- unique(frame$year)
  
  #Look for mask
  mask_files <- path_scenes(mask_path)
  mask_files <- mask_files[, 1:2]
  mask_files <- mask_files[, year := strsplit(scene, "_")[[1]][1], 
                           by = seq_along(1:nrow(mask_files))]
  mask_files$year <- as.integer(mask_files$year)
  
  #Progress bar
  pb <- txtProgressBar(min = 1, 
                       max = length(unique_tile)*length(unique_years), 
                       style = 3)
  
  #Loop over tiles
  for(i in 1:length(unique_tile)) {
    
    #Subset to work with a given tile
    sub_tile <- subset(frame, tile == unique_tile[i])
    
    #Loop over years
    for(ii in 1:length(unique_years)) { 
      
      #Progress
      setTxtProgressBar(pb, i*ii)
      
      #Subset layers for a given year
      sub_year <- subset(sub_tile, year(date) == unique_years[ii])
      
      if(nrow(sub_year) >= 5) {
        
        ###Look for overwrite---------------------------------------------------
        
        slope_name <- paste0(out_path, "/", 
                              unique_tile[i], "/", 
                              unique_years[ii], "_", 
                              band_select, "_",
                              "slope", ".tif")
        
        exists_layer <- file.exists(slope_name)
        
        if(exists_layer == TRUE & overwrite == FALSE) {
          next
        } 
        
        ###Mask-----------------------------------------------------------------
        mask <- mask_files[tile == unique_tile[i] & year == unique_years[ii]]
        mask_name <- paste0(mask_path, "/", mask$tile, "/", mask$scene)
        mask_layer  <- rast(mask_name)
        
        ###Trend----------------------------------------------------------------
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
        
        ###Export---------------------------------------------------------------
        #Look for the directory
        directory <- paste0(out_path, "/", 
                            unique_tile[i])
        
        #Create directory
        if(!dir.exists(directory)) {
          dir.create(directory)
        }
        
        #Export raster
        writeRaster(trend_layer, 
                    slope_name, 
                    names = "slope", 
                    overwrite=TRUE,
                    NAflag = -9999)
        
        #Release memory -just in case-
        gc()
        
      } else{
        cat(paste0("No ", unique_tile[i], " for ", unique_years[ii]))
      }
    }
  }
}

#' @example 
trends_sen2(root_path, evaluation_doy, band_select, mask_path, out_path, overwrite = FALSE, threads)



