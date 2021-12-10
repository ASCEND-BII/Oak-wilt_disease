################################################################################
##### Create mask
################################################################################

#' This function create a mask layer per year based on kNDVI that is used to
#' filter vegetation to non-vegetation

#' -----------------------------------------------------------------------------
# Source load

source("R/00-path_scenes.R")
source("R/00-get_mask.R")

#' -----------------------------------------------------------------------------
#' Libraries

library(data.table)
library(terra)

#' -----------------------------------------------------------------------------
#' Arguments

#' @param root_path select the root path of where the scenes are located
#' @param mask_months period in which mask will be estimated for
#' @param out_path path of the outputs
#' @param overwrite if the file exist, do you want to create it again?

root_path <- "/media/antonio/Work/Oak-wilt/level3_sen3"
mask_doy <- c(152, 212) #The month of june
out_path <- "/media/antonio/Work/Oak-wilt/level4_mask" 

#'------------------------------------------------------------------------------
#' Function
create_mask <- function(root_path, mask_doy, out_path, overwrite = FALSE) {
  
  #Get scenes to work with
  frame <- path_scenes(root_path)
  
  #Subset just to KNV
  frame <- frame[band == "KNV"]
  
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
  n <- 1
  
  #Loop over tiles
  for(i in 1:length(unique_tile)) {
    
    #Subset to work with a given tile
    sub_tile <- subset(frame, tile == unique_tile[i])
    
    #Loop over years
    for(ii in 1:length(unique_years)) { 
      
      #Progress
      setTxtProgressBar(pb, n)
      n <- n+1
      
      #Overwrite
      layer_name <- paste0(out_path, "/", 
                           unique_tile[i], "/", 
                           unique_years[ii], "_", 
                           "mask.tif")
      
      exists_layer <- file.exists(layer_name)
      
      if(exists_layer == TRUE & overwrite == FALSE) {
        next
      }
      
      #Subset layers for a given year
      sub_year <- subset(sub_tile, year(date) == unique_years[ii])
      
      ###Mask-------------------------------------------------------------------
      mask_scenes <- subset(sub_year, doy >= mask_doy[1] & doy <= mask_doy[2])
      
      if(length(mask_scenes) >= 3) {
        
        mask_scenes <- paste0(root_path, "/", 
                              mask_scenes$tile, "/", mask_scenes$scene)
        
        #Mask
        mask_layer <- get_mask(mask_scenes, threshold = 0.55)
        
        #Export mask
        directory <- paste0(out_path, "/", 
                            unique_tile[i])
        
        #Create directory
        if(!dir.exists(directory)) {
          dir.create(directory)
        }
        
        writeRaster(mask_layer, 
                    layer_name, 
                    names = "mask", 
                    overwrite=TRUE,
                    NAflag = -9999)
        
      } else {
        
        cat(paste0("No ", unique_tile[i], " for ", unique_years[ii]))
        
      }
    }
  }
}


#' @example 
create_mask(root_path, mask_doy, out_path, overwrite = FALSE)
