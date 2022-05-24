################################################################################
##### Estimation of trends using Sentinel-2 time series derived from FORCE
################################################################################

#-------------------------------------------------------------------------------
# Source load

source("R/00-path_scenes.R")
source("R/00-predict_sam.R")

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
band_select <- c("CRE", "NDM")
mask_path <- "/media/antonio/Work/Oak-wilt/level4_mask" 
out_path <- "/media/antonio/Work/Oak-wilt/level4_sen2"
threads <- 28

#-------------------------------------------------------------------------------
#Function
create_sam <- function(root_path, evaluation_doy, band_select, mask_path, out_path, overwrite = FALSE, threads) {
  
  #Get scenes to work with
  frame <- path_scenes(root_path)
  
  unique(frame[,c("title","date")])
  
  #Unique tiles
  unique_tile <- unique(frame$tile)
  
  #Look for mask
  mask_files <- path_scenes(mask_path)
  mask_files <- mask_files[, 1:2]
  mask_files <- mask_files[, year := strsplit(scene, "_")[[1]][1], 
                           by = seq_along(1:nrow(mask_files))]
  mask_files$year <- as.integer(mask_files$year)
  
  #Progress bar
  pb <- txtProgressBar(min = 1, 
                       max = nrow(frame)/2, 
                       style = 3)
  n <- 1
  
  #Loop over tiles
  for(i in 1:length(unique_tile)) {
    
    #Subset to work with a given tile
    sub_tile <- subset(frame, tile == unique_tile[i])
    
    #Mask to use
    mask_frame <- subset(mask_files, tile == unique_tile[i])
    
    #Unique date
    unique_date <- unique(sub_tile$date)
    
    #Loop over years
    for(ii in 1:length(unique_date)) { 
      
      #Progress
      setTxtProgressBar(pb, n)
      n <- n+1
      
      #Subset layers for a given date
      sub_date <- subset(sub_tile, date == unique_date[ii])
      
      #Read mask----------------------------------------------------------------
      year_mask <- unique(sub_date$year)
      mask_to_use <- subset(mask_frame, year == year_mask)
      mask_name <- paste0(mask_path, "/", mask_to_use$tile, "/", mask_to_use$scene)
      mask_layer  <- rast(mask_name)
      
      #Read VI------------------------------------------------------------------
      
      #Names of VI and load
      CRE_name <- subset(sub_date, band == "CRE")
      CRE_name <- paste0(root_path, "/", CRE_name$tile, "/", CRE_name$scene)
      
      NDM_name <- subset(sub_date, band == "NDM")
      NDM_name <- paste0(root_path, "/", NDM_name$tile, "/", NDM_name$scene)
      
      vi_files <- c(CRE_name, NDM_name)
      VI <- rast(vi_files)  
      names(VI) <- c("CIre", "NDBI")  
      
      #Apply mask---------------------------------------------------------------
      
      scenes_masked <- mask(VI,
                            mask_layer,
                            maskvalues= 0)
      
      #Predict SAM---------------------------------------------------------------
      
      #No parallel so far
      sam_predicted <- predfun(scenes_masked, 
                               healty_model, 
                               wilted_model, 
                               dead_model)
      
      ###Export---------------------------------------------------------------
      
      #Look for the directory
      directory <- paste0(out_path, "/", unique_tile[i])
      
      #File name
      layer_name <- paste0(directory, "/", unique_date[ii], "_sam.tif")
      
      #Create directory
      if(!dir.exists(directory)) {
        dir.create(directory)
      }
      
      #Export raster
      writeRaster(sam_predicted, 
                  layer_name, 
                  names = c("healty", "wilted", "dead"), 
                  overwrite = TRUE,
                  NAflag = -9999)
      
      #Release memory -just in case-
      gc()
      
    }
  }
}

#' @example 
trends_sen2(root_path, evaluation_doy, band_select, mask_path, out_path, overwrite = FALSE, threads)



