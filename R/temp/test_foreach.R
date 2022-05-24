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
library(foreach)
library(doSNOW)

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
threads <- 16

#-------------------------------------------------------------------------------
#Function
create_sam <- function(root_path, evaluation_doy, band_select, mask_path, out_path, overwrite = FALSE, threads) {
  
  #Get scenes to work with
  frame <- path_scenes(root_path)
  
  #Unique values
  unique_scenes <- unique(frame[,list(tile, date)])
  
  #Look for mask
  mask_files <- path_scenes(mask_path)
  mask_files <- mask_files[, 1:2]
  mask_files <- mask_files[, year := strsplit(scene, "_")[[1]][1], 
                           by = seq_along(1:nrow(mask_files))]
  mask_files$year <- as.integer(mask_files$year)
  
  #Progress bar
  cl <- makeCluster(threads, outfile="") #Make clusters
  registerDoSNOW(cl)
    
  pb <- txtProgressBar(min = 0, max = nrow(unique_scenes), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  
  #nrow(unique_scenes)
  #Loop over scenes
  results <- foreach(i = 1:nrow(unique_scenes), 
                     .inorder = FALSE, 
                     .packages = c("data.table", "terra"), 
                     .export=c("predfun", "healty_model", "wilted_model", "dead_model"), 
                     .options.snow = opts) %dopar% {
    
    sub_scenes <- subset(frame, tile == unique_scenes$tile[i] & 
                                date == unique_scenes$date[i])
    
    band <- c("BLU", "GRN", "RED", "RE1", 
              "RE2", "RE3", "NIR", 
              "SW1", "SW2")
    
    bands <- data.table(band = band)
    
    sub_scenes <- merge(bands, sub_scenes, by = "band", 
                        all.x = TRUE, all.y = FALSE)
  
    sub_scenes$band <- factor(sub_scenes$band, levels = band)
    sub_scenes <- sub_scenes[order(ordered(band))]
    
    #Load raster
    scene <- rast(paste0(root_path, "/", 
                         sub_scenes$tile, "/",
                         sub_scenes$scene))
    names(scene) <- c("BLUE", "GREEN", "RED", "REDEDGE1",
                      "REDEDGE2", "REDEDGE3",  
                      "NIR", "SWIR1", "SWIR2")
    
    # Load mask-----------------------------------------------------------------
    mask_to_use <- subset(mask_files, year == unique(sub_scenes$year) &
                                      tile == unique(sub_scenes$tile))
    mask_name <- paste0(mask_path, "/", mask_to_use$tile, "/", mask_to_use$scene)
    mask_layer <- rast(mask_name)
    
    # Apply mask----------------------------------------------------------------
    
    scenes_masked <- mask(scene,
                          mask_layer,
                          maskvalues= 0)
    
    #Predict SAM---------------------------------------------------------------
    
    sam_predicted <- predfun(scenes_masked, 
                             healty_model, 
                             wilted_model, 
                             dead_model)
    
    #Look for the directory
    directory <- paste0(out_path, "/", unique(sub_scenes$tile))
    
    #File name
    layer_name <- paste0(directory, "/", unique(sub_scenes$date), "_sam.tif")
    
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
    rm(c("sub_scenes", "sam_predicted", "band", "bands", 
         "mask_layer", "scenes_masked", "layer_name"))
    gc()
    
    return(0)
  }
  
  stopCluster(cl)
  gc()
    
}

#' @example 
create_sam(root_path, evaluation_doy, band_select, mask_path, out_path, overwrite = FALSE, threads)



