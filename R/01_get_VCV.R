################################################################################
##### Estimation of VCV 
################################################################################

#' @description A function to estimate value of coefficient of variance.

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(terra)
library(parallel)

#-------------------------------------------------------------------------------
#Arguments

#' @param root_path: select the root path of where the scenes are located
#' @param threads: the number of threads to use for parallel processing

#path <- "/media/antonio/Work/Projects/Oak-wilt_mapping"
#root_path <- paste0(path, "/level3_application")
#get_VCV(root_path)

#-------------------------------------------------------------------------------
#Arguments

get_VCV <- function(root_path, threads = 16) {
  
  #--------------------------------------------------------
  # Selection and arrangement of files
  
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
  
  # Remove potential .ovr
  frame[, ovr := strsplit(scene, ".POL")[[1]][2], by = seq_along(1:nrow(frame))]
  frame <- subset(frame, ovr == ".tif")
  
  #Unique date and tile
  frame[, unique := .GRP, by=.(tile, VI)]
  
  #N scenes
  n_scenes <- uniqueN(frame$unique)
  
  #--------------------------------------------------------
  #Function for batch application
  application <- function(i, frame) {
    
    #tile
    tile <- subset(frame, unique == i)
    
    #Files
    VGV <- rast(paste0(root_path, "/", tile$tile[1], "/", tile[metric == "VGV", scene]))
    VGM <- rast(paste0(root_path, "/", tile$tile[1], "/", tile[metric == "VGM", scene]))
    
    #Get index
    VCV <- (VGV/VGM)*1000
    VCV[is.infinite(VCV)] <- NA
    
    #export name
    exp_name <- strsplit(tile[metric == "VGM", scene], "VGM")[[1]]
    export_name <- paste0(root_path, "/", tile$tile[1], "/", exp_name[1], "VCV", exp_name[2])
    
    #write raster
    writeRaster(VCV, 
                export_name, 
                overwrite = TRUE, 
                names = names(VCV),
                NAflag = -9999,
                datatype = "INT2S")
    
    #Remove residuals
    rm(list = c("tile", "VGV", "VGM", "exp_name", "export_name"))
    Sys.sleep(2)
    
  }
  
  # Parallel
  mclapply(1:n_scenes, 
           FUN = application, 
           frame = frame, 
           mc.cores = threads,
           mc.preschedule = FALSE,
           mc.cleanup = FALSE)

}

#-------------------------------------------------------------------------------
#' @example 

root_path <- "/panfs/jay/groups/17/cavender/shared/oak-wilt/level3"
get_VCV(root_path, threads = 24)