################################################################################
##### Change projection
################################################################################

#-------------------------------------------------------------------------------
# Source load

source("R/00-path_vi_scenes.R")

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(terra)
library(doParallel)
library(foreach)

#-------------------------------------------------------------------------------
#Arguments

#' @param root_path: select the root path of where the scenes are located
#' @param out_path: path of the outputs
#' @param threads: the number of threads to use for parallel processing

root_path <- "F:/FORCE/level3"
out_path <- "F:/FORCE/level3_shifted"
threads <- 10

#-------------------------------------------------------------------------------
#Arguments

change_projection <- function(root_path, out_path, threads = 26) {
  
  #Get scenes to work with
  frame <- path_vi_scenes(root_path)
  
  #Get just RED and GRN
  frame <- subset(frame, VI == "CCI" |  
                         VI == "CRE" | 
                         VI == "NDW")
  
  #Unique date and tile
  frame <- subset(frame, year == "2019")
  
  #N scenes
  n_scenes <- nrow(frame)
  
  # Set up cluster
  cl <- makeCluster(threads, type = "FORK")
  registerDoParallel(cl)
  
  # Loop over scenes to estimate kNDVI
  foreach(i = 1:n_scenes,
          .packages = c("terra", "data.table"),
          .inorder = F) %dopar% {
            
            #Read raster
            raster <- rast(paste0(root_path, "/", frame$tile[i], "/", frame$scene[i]))
            
            #Project
            new_raster <- project(raster, "EPSG:26915")
            
            #write raster
            writeRaster(new_raster, 
                        paste0(out_path, "/", frame$tile[i], "/", frame$scene[i]), 
                        overwrite = TRUE, 
                        names = names(new_raster),
                        NAflag = -9999)
            
            #Remove residuals
            rm(list = c("raster", "new_raster"))
            gc()
            
          }
  
  #Stop cluster
  stopCluster(cl)
  gc()
  
}
#' @example 
root_path <- "/home/cavender/shared/oak-wilt/level3"
out_path <- "/home/cavender/shared/oak-wilt/level3"
threads <- 8

change_projection(root_path, out_path, threads = 6)
