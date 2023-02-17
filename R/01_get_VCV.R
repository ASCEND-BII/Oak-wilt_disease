################################################################################
##### Estimation of VCV 
################################################################################

#' @description A function to estimate value of coefficient of variance.

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
get_VCV(root_path)

#-------------------------------------------------------------------------------
#Arguments

get_VCV <- function(root_path) {
  
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
  frame <- subset(frame, metric == "VGV" | metric == "VGM")
  
  #Unique date and tile
  frame[, unique := .GRP, by=.(tile, VI)]
  
  #N scenes
  n_scenes <- uniqueN(frame$unique)
  
  for(i in 1:n_scenes) {
    
    #tile
    tile <- subset(frame, unique == i)
    
    #Files
    VGV <- rast(paste0(root_path, "/", tile$tile[1], "/", tile[metric == "VGV", scene]))
    VGM <- rast(paste0(root_path, "/", tile$tile[1], "/", tile[metric == "VGM", scene]))
    
    #Get index
    VCV <- VGV/VGM*1000
    VCV[is.infinite(VCV)] <- 0
    
    #export name
    exp_name <- strsplit(tile[metric == "VGM", scene], "VGM")[[1]]
    export_name <- paste0(root_path, "/", tile$tile[1], "/", exp_name[1], "VCV", exp_name[2])
    
    #write raster
    writeRaster(VCV, 
                export_name, 
                overwrite = TRUE, 
                names = names(VCV),
                NAflag = -9999)
    
  }
}
