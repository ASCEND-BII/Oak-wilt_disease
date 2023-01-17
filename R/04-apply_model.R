################################################################################
##### 04 - Predict models
################################################################################

#' @description Batch for prediction of the models using the LSP scenes.

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(terra)
library(parallel)

#-------------------------------------------------------------------------------
# Source code

source("R/00-model_predict.R")

#-------------------------------------------------------------------------------
# Root path
path <- "/media/antonio/Work/Projects/Oak-wilt_mapping"

#-------------------------------------------------------------------------------
#Arguments

#' @param root_path: select the root path of where the scenes are located
#' @param models Models for wilted, healhty, and dead.
#' @param out_path: path and name of the .txt outputs
#' @param threads: the number of threads to use for parallel processing

root_path <- paste0(path, "/level3_application")
models <- readRDS("data/models/final_model.rds")
out_path <- paste0(path, "/level4")
threads <- 16

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
  
  frame <- frame[order(tile, metric)]
  
  #Subset metric
  frame <- subset(frame, metric == "VSS" |
                         metric == "VGM" |
                         metric == "VGV" |
                         metric == "VPS")
  
  #Get date
  date <- rast(paste0(root_path, "/", files[1]))
  date <- names(date)
  
  #Get unique tiles
  unique_tile <- unique(frame$tile)
  
  #Creates function
  application <- function(i, frame, models) {
  
    sub_frame <- frame[tile == unique_tile[i]]
    
    #Read scenes -------------------
    VSS <- sub_frame[metric == "VSS" & VI == "CCI"]
    VSS <- rast(paste0(root_path, "/",
                       VSS$tile[1], "/", 
                       VSS$scene[1]))
    
    VGM <- sub_frame[metric == "VGM" & VI == "CCI"]
    VGM <- rast(paste0(root_path, "/",
                       VGM$tile[1], "/", 
                       VGM$scene[1]))
    
    VGV <- sub_frame[metric == "VGV" & VI == "CCI"]
    VGV <- rast(paste0(root_path, "/",
                       VGV$tile[1], "/", 
                       VGV$scene[1]))
    
    VPS <- sub_frame[metric == "VPS" & VI == "KNV"]
    VPS <- rast(paste0(root_path, "/",
                       VPS$tile[1], "/", 
                       VPS$scene[1]))
    
    for(j in 1:length(date)) {
      
      #Mask KNDVI
      mask <- VPS[date[j]]
      mask[mask < 4000] <- 0
      mask[mask >= 4000] <- 1
      
      #Get VCV
      VCV <- VGV[date[j]]/VGM[date[j]]
      VCV[VCV == Inf] <- NA
      
      #Get values per mask
      vss <- mask(VSS[date[j]], mask, maskvalues = 0)
      vgv <- mask(VGV[date[j]], mask, maskvalues = 0)
      vcv <- mask(VCV, mask, maskvalues = 0)
      
      #Get normalization values
      vss_mean <- mean(vss[], na.rm = TRUE)
      vgv_mean <- mean(vgv[], na.rm = TRUE)
      vcv_mean <- mean(vcv[], na.rm = TRUE)
      
      vss_sd <- sd(vss[], na.rm = TRUE)
      vgv_sd <- sd(vgv[], na.rm = TRUE)
      vcv_sd <- sd(vcv[], na.rm = TRUE)
      
      #Apply z-score normalization
      vss <- (VSS[date[j]] - vss_mean)/vss_sd
      vgv <- (VGV[date[j]] - vgv_mean)/vgv_sd
      vcv <- (VCV - vcv_mean)/vcv_sd
      
      #Change names
      names(vss) <- "VSS"
      names(vgv) <- "VGV"
      names(vcv) <- "VCV"
      
      #Stack scene
      scene <- c(vss,
                 vgv,
                 vcv)

      #Apply models and get uncertainties
      predictions <- model_predict(scene, models)
      
      #Export name
      export_predicted <- paste0(out_path, "/", 
                                 sub_frame$tile[1], "/",
                                 date[j], "_predicted.tif")

      export_uncertainty <- paste0(out_path, "/", 
                                   sub_frame$tile[1], "/",
                                   date[j], "_uncertainty.tif")
      
      #Export
      writeRaster(x = round(predictions$Predicted*10000, 0),
                  filename = export_predicted,
                  names = c("Healthy", "Symptomatic", "Dead"),
                  NAflag = NA,
                  overwrite= TRUE)
      
      writeRaster(x = round(predictions$Uncertainty*10000, 0),
                  filename = export_uncertainty,
                  names = c("Healthy", "Symptomatic", "Dead"),
                  NAflag = NA,
                  overwrite= TRUE)
      
      
      #Remove residuals
      rm(list = c("mask", "VCV", 
                  "vss", "vgv", "vcv",
                  "vss_mean", "vgv_mean", "vcv_mean",
                  "vss_sd", "vgv_sd", "vcv_sd",
                  "scene", "predictions",
                  "export_predicted", "export_uncertainty"))
      
    }
    
    #Remove residuals
    rm(list = c("sub_frame", "VSS", 
                "VGM", "VGV", "VPS"))
    gc()
    
    return(NA)
    
  }
  
  # Parallel
  mclapply(1:length(unique_tile), 
           FUN = application, 
           frame = frame, 
           models = models, 
           mc.cores = 16,
           mc.preschedule = TRUE,
           mc.cleanup = TRUE,
           )
  
}
