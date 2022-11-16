################################################################################
##### 06 - Predict models
################################################################################

#' @description Batch for prediction of the models in the dVI scenes.

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(terra)
library(doParallel)
library(foreach)

#-------------------------------------------------------------------------------
# Source code

source("R/00-model_predict.R")

#-------------------------------------------------------------------------------
# Root path
path <- "/media/antonio/antonio_ssd/FORCE/corregistration/application"
path <- "F:/TRAINING"

#-------------------------------------------------------------------------------
#Arguments

#' @param root_path: select the root path of where the scenes are located
#' @param models Models for wilted, healhty, and dead.
#' @param out_path: path and name of the .txt outputs
#' @param threads: the number of threads to use for parallel processing

root_path <- paste0(path, "/level3_lsf")
models <- readRDS("data/models/final_model.rds")
out_path <- paste0(path, "/level4")
threads <- 2

apply_model(root_path, models, out_paths, threads)

#-------------------------------------------------------------------------------
#Function

apply_model <- function(root_path, models, out_paths, threads) {
  
  #Search for VI paths
  files <- list.files(path = root_path, 
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
  
  #Get method
  frame[, metric := substr(strsplit(scene, "_")[[1]][7], 1, 3), by = seq_along(1:nrow(frame))]
  
  #Subset VI
  #frame <- frame[VI == "CCI"]
  frame <- frame[order(tile, metric)]
  
  #Subset metric
  frame <- subset(frame, metric == "VSS" |
                         metric == "IFR" |
                         metric == "VGM" |
                         metric == "VGV" |
                         metric == "VPS" |
                         metric == "VEV")
  
  #Get date
  date <- rast(paste0(root_path, "/", files[1]))
  date <- names(date)
  
  #Get unique tiles
  unique_tile <- unique(frame$tile)
  
  # Set up cluster
  cl <- makeCluster(threads, type = "FORK")
  registerDoParallel(cl)
  
  # Loop over scenes to estimate kNDVI
  for(i in 1:nrow(unique_tile) {
    
    sub_frame <- frame[tile == unique_tile[i]]
    
    #Read scenes -------------------
    VSS <- sub_frame[metric == "VSS" & VI == "CCI"]
    VSS <- rast(paste0(root_path, "/",
                       VSS$tile[1], "/", 
                       VSS$scene[1]))
    
    IFR <- sub_frame[metric == "IFR" & VI == "CCI"]
    IFR <- rast(paste0(root_path, "/",
                       IFR$tile[1], "/", 
                       IFR$scene[1]))
    
    VGM <- sub_frame[metric == "VGM" & VI == "CCI"]
    VGM <- rast(paste0(root_path, "/",
                       VGM$tile[1], "/", 
                       VGM$scene[1]))
    
    VGV <- sub_frame[metric == "VGV" & VI == "CCI"]
    VGV <- rast(paste0(root_path, "/",
                       VGV$tile[1], "/", 
                       VGV$scene[1]))
    
    VPS <- sub_frame[metric == "VPS" & VI == "CCI"]
    VPS <- rast(paste0(root_path, "/",
                       VPS$tile[1], "/", 
                       VPS$scene[1]))
    
    KNDVI <- sub_frame[metric == "VSS" & VI == "KNV"]
    KNDVI <- rast(paste0(root_path, "/",
                         KNDVI$tile[1], "/", 
                         KNDVI$scene[1]))
    
    for(j in 1:length(date)) {
      
      #Mask KNDVI
      mask <- KNDVI[date[j]]
      mask[mask < 3500] <- 0
      mask[mask >= 3500] <- 1
      
      #Create stack
      VCV <- VGV[date[j]]/VGM[date[j]]
      VCV[VCV == Inf] <- NA
      PPM <- (VPS[date[j]]-VGM[date[j]])/VGM[date[j]]
      PPM[PPM == Inf] <- NA
      
      scene <- c(PPM,
                 VSS[date[j]],
                 VCV,
                 IFR[date[j]])
      
      names(scene) <- c("PPM", "VSS", "VCV", "IFR")
      
      #Mask stack
      scene <- mask(scene, mask, maskvalues = 0)
      
      #LDA Model application -----------------------------
      predictions <- predict(scene, model = models$SVM_1, 
                             type = "prob", cores = 4, 
                             na.rm = TRUE, cpkgs = c("caret"))
      
      
      mean <- mean()
      sd = 
      
      #Export model
      export_mean <- paste0(out_path, "/", 
                            sub_frame$tile[1], "/",
                            date[j], "_predicted_mean.tif")

      export_sd <- paste0(out_path, "/", 
                          sub_frame$tile[1], "/",
                          date[j], "_predicted_SD.tif")
      
      #Export
      writeRaster(x = predicted_mean,
                  filename = export_name,
                  names = c("Healthy", "Symptomatic", "Dead"),
                  NAflag = 0,
                  overwrite= TRUE)
      
      writeRaster(x = predicted_sd,
                  filename = export_name,
                  names = c("Healthy", "Symptomatic", "Dead"),
                  NAflag = 0,
                  overwrite= TRUE)
      
      gc()
      
    }
    
    #Remove residuals
    #rm(list = c("scene", "healthy", "wilted", "dead",
    #            "kNDVI", "predicted", "export_name"))
                          
                          
  }
  
  
}
