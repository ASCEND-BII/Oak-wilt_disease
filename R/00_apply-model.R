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

path <- "/media/antonio/antonio_ssd/FORCE/aplication"

#-------------------------------------------------------------------------------
#Arguments

#' @param root_path: select the root path of where the scenes are located
#' @param models Models for wilted, healhty, and dead.
#' @param out_path: path and name of the .txt outputs
#' @param threads: the number of threads to use for parallel processing

root_path <- paste0(path, "/level3")
models <- readRDS("data/models/models.rds")
out_path <- paste0(path, "/level4")
threads <- 24

apply_model(root_path, models, out_paths, threads)

#-------------------------------------------------------------------------------
#Function

apply_model <- function(root_path, models, out_paths, year, threads) {
  
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
  frame[, VI := substr(scene, 32, 34), by = seq_along(1:nrow(frame))]
  
  #Get date
  date <- rast(paste0(root_path, "/", files[1]))
  date <- names(date)
  date <- as.IDate(c(paste0(substr(date, 1, 4), "-", substr(date, 5, 6), "-", substr(date, 7, 8))))
  years <- year(date)
  
  #Unique tiles
  unique_tiles <- unique(frame$tile)
  unique_years <- unique(years)
  
  application <- function(i, unique_tiles, date, years, unique_years, root_path, out_path) {
    
    #Read VIs
    CCI <- rast(paste0(root_path, "/", unique_tiles[i], "/", "2017-2021_001-365_HL_TSA_SEN2L_CCI_TSI.tif"))
    CRE <- rast(paste0(root_path, "/", unique_tiles[i], "/", "2017-2021_001-365_HL_TSA_SEN2L_CRE_TSI.tif"))
    NDW <- rast(paste0(root_path, "/", unique_tiles[i], "/", "2017-2021_001-365_HL_TSA_SEN2L_NDW_TSI.tif"))
    KNV <- rast(paste0(root_path, "/", unique_tiles[i], "/", "2017-2021_001-365_HL_TSA_SEN2L_KNV_TSI.tif"))
    
    
    
    for(j in 1:length(unique_years)) {
      
      CCI_year <- CCI[years == unique_years[j]]
      CRE_year <- CRE[years == unique_years[j]]
      NDW_year <- NDW[years == unique_years[j]]
      KNV_year <- KNV[years == unique_years[j]]
      
      predicted <- model_predict(CCI_year, CRE_year, NDW_year, KNV_year, model)
      
      
    }
    
  }
  
  
  # Set up cluster
  cl <- makeCluster(threads, type = "FORK")
  registerDoParallel(cl)
  
  # Loop over scenes to estimate kNDVI
  extraction <- foreach(i = 1:nrow(frame),
                        .combine = rbind,
                        .packages = c("terra", "data.table"),
                        .inorder = F) %dopar% {
                          
                          #Read scene -------------------
                          scene <- rast(paste0(root_path, "/",
                                               frame$tile[i], "/", 
                                               frame$scene[i]))
                          
                          #Model application ------------
                          # Healthy
                          healthy <- model_predict(scene, models$healthy)
                          
                          # Wilted
                          wilted <- model_predict(scene, models$wilted)
                          
                          # Dead
                          dead <- model_predict(scene, models$dead)
                          
                          #Predicted scene
                          predicted <- c(healthy, wilted, dead)
                          names(predicted) <- c("healthy", "wilted", "dead")
                          
                          #Mask
                          kNDVI <- scene$kND
                          kNDVI[kNDVI < 3000] <- 0
                          kNDVI[kNDVI >= 3000] <- 1
                          predicted <- mask(predicted, kNDVI, maskvalues = 0)
                          
                          #plotRGB(predicted, r = 2, g = 1, b = 3, stretch = TRUE)
                          
                          export_name <- paste0(out_path, "/", 
                                                frame$tile[i], "/",
                                                frame$year[i], "_predicted.tif")
                          
                          #Export
                          writeRaster(x = predicted,
                                      filename = export_name,
                                      names = c("healthy", "wilted", "dead"),
                                      NAflag = NA,
                                      overwrite= TRUE)
                          
                          #Remove residuals
                          rm(list = c("scene", "healthy", "wilted", "dead",
                                      "kNDVI", "predicted", "export_name"))
                          gc()
                          
                        }
  
  #Stop cluster
  stopCluster(cl)
  gc()
  
}
