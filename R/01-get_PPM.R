################################################################################
##### Estimation of kNGRDI 
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

path <- "/media/antonio/Work/Projects/Oak-wilt_mapping"
root_path <- paste0(path, "/level3_application")
get_PPM(root_path)

#-------------------------------------------------------------------------------
#Arguments

get_PPM <- function(root_path) {
  
  #Search for paths
  files <- list.files(path = paste0(root_path), 
                      pattern = ".tif", 
                      all.files = TRUE,
                      full.names = FALSE, 
                      recursive = TRUE)
  
  #Arrange path in frame
  frame <- data.table(matrix(unlist(strsplit(files, "/")), 
                             nrow= length(files), 
                             byrow=TRUE), stringsAsFactors=FALSE)
  colnames(frame) <- c("tile", "scene")
  
  #Get VI
  frame[, VI := strsplit(scene, "_")[[1]][6], by = seq_along(1:nrow(frame))]
  
  # Get metric
  frame[, metric := substr(strsplit(scene, "_")[[1]][7], 1, 3), by = seq_along(1:nrow(frame))]
  
  # Get metrics and VI of interest
  frame <- subset(frame, VI == "CCI")
  frame <- subset(frame, metric == "VPS" | metric == "VGM")
  
  #Unique date and tile
  frame[, unique := .GRP, by=.(tile)]
  
  #N scenes
  n_scenes <- uniqueN(frame$unique)
  
  for(i in 1:n_scenes) {
    
    #tile
    tile <- subset(frame, unique == i)
    
    #Files
    VPS <- rast(paste0(root_path, "/", tile$tile[1], "/", tile[metric == "VPS", scene]))
    VGM <- rast(paste0(root_path, "/", tile$tile[1], "/", tile[metric == "VGM", scene]))
    
    #Get index
    PPM <- ((VPS - VGM) / VGM)*1000
    
    #export name
    exp_name <- strsplit(tile[metric == "VPS", scene], "VPS")[[1]]
    export_name <- paste0(root_path, "/", tile$tile[1], "/", exp_name[1], "PPM", exp_name[2])
    
    #write raster
    writeRaster(PPM, 
                export_name, 
                overwrite = TRUE, 
                names = names(PPM),
                datatype = "INT16S",
                NAflag = -9999)
    
  }
}
