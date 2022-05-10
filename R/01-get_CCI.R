################################################################################
##### Estimation of CCI
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
out_path <- "F:/FORCE/level3"
threads <- 10

#-------------------------------------------------------------------------------
#Arguments

get_CCI <- function(root_path, out_path, threads = 26) {
  
  #Get scenes to work with
  frame <- path_vi_scenes(root_path)
  
  #Get just RED and GRN
  frame <- subset(frame, VI == "RED" |  VI == "GRN" )
  
  #Unique date and tile
  frame[, unique := .GRP, by=.(tile, date)]
  
  #N scenes
  n_scenes <- uniqueN(frame$unique)
  
  # Set up cluster
  cl <- makeCluster(threads, type = "FORK")
  registerDoParallel(cl)
  
  # Loop over scenes to estimate kNDVI
  foreach(i = 1:n_scenes,
          .packages = c("terra", "data.table"),
          .inorder = F) %dopar% {
          
          #Files
          GREEN <- frame[VI == "GRN" & unique == i]
          RED <- frame[VI == "RED" & unique == i]
          
          #Read raster
          rGREEN <- rast(paste0(root_path, "/", GREEN$tile[1], "/", GREEN$scene[1]))
          rRED <- rast(paste0(root_path, "/", RED$tile[1], "/", RED$scene[1]))
          
          #Get index
          CCI <- round(((rGREEN - rRED)/(rGREEN + rRED)) * 10000, 0)
          
          #export name
          export_name <- strsplit(GREEN$scene[1], "GRN")[[1]]
          export_name <- paste0(out_path, "/", 
                                GREEN$tile[1], "/", 
                                export_name[1], "CCI", export_name[2])
          
          #write raster
          writeRaster(CCI, 
                      export_name, 
                      overwrite = TRUE, 
                      names = GREEN$date[1],
                      datatype = "INT16S",
                      NAflag = -9999)
          
          #Remove residuals
          rm(list = c("GREEN", "RED", "rGREEN", "rRED", "CCI", "export_name"))
          gc()
          
          }
  
  #Stop cluster
  stopCluster(cl)
  gc()
  
}
#' @example 

root_path <- "F:/FORCE/level3"
out_path <- "F:/FORCE/level3"
threads <- 100

get_CCI(root_path, out_path, threads = threads)