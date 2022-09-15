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
path <- "E:/FORCE/corregistration/application"

#-------------------------------------------------------------------------------
#Arguments

#' @param root_path: select the root path of where the scenes are located
#' @param models Models for wilted, healhty, and dead.
#' @param out_path: path and name of the .txt outputs
#' @param threads: the number of threads to use for parallel processing

root_path <- paste0(path, "/level3_shifted_phenology")
models <- readRDS("data/models/models.rds")
out_path <- paste0(path, "/level4_shifted")
threads <- 24

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
  #frame <- subset(frame, metric == "VGV" |
  #                  metric == "VSS" |
  #                  metric == "VPA" |
  #                  metric == "IGS")
  
  #Get date
  date <- rast(paste0(root_path, "/", files[1]))
  date <- names(date)
  
  #Get unique tiles
  unique_tile <- unique(frame$tile)
  
  # Set up cluster
  cl <- makeCluster(threads, type = "FORK")
  registerDoParallel(cl)
  
  # Loop over scenes to estimate kNDVI
  extraction <- foreach(i = 1:nrow(unique_tile),
                        .combine = rbind,
                        .packages = c("terra", "data.table"),
                        .inorder = F) %dopar% {
                          
                          sub_frame <- frame[tile == unique_tile[i]]
                          
                          #Read scenes -------------------
                          IGS <- sub_frame[metric == "IGS" & VI == "CCI"]
                          IGS <- rast(paste0(root_path, "/",
                                             IGS$tile[1], "/", 
                                             IGS$scene[1]))
                          
                          VGV <- sub_frame[metric == "VGV" & VI == "CCI"]
                          VGV <- rast(paste0(root_path, "/",
                                             VGV$tile[1], "/", 
                                             VGV$scene[1]))
                          
                          VPA <- sub_frame[metric == "VPA" & VI == "CCI"]
                          VPA <- rast(paste0(root_path, "/",
                                             VPA$tile[1], "/", 
                                             VPA$scene[1]))
                          
                          VSS <- sub_frame[metric == "VSS" & VI == "CCI"]
                          VSS <- rast(paste0(root_path, "/",
                                             VSS$tile[1], "/", 
                                             VSS$scene[1]))
                          
                          KNDVI <- sub_frame[metric == "VPS" & VI == "KNV"]
                          KNDVI <- rast(paste0(root_path, "/",
                                               KNDVI$tile[1], "/", 
                                               KNDVI$scene[1]))
                          
                          for(j in 1:length(date)) {
                            
                            scene <- c(VGV[date[j]],
                                       VSS[date[j]],
                                       VPA[date[j]],
                                       IGS[date[j]])
                            names(scene) <- c("VGV", "VSS", "VPA", "IGS")
                            
                            #LDA Model application -----------------------------
                            classes <- predict(scene, model = models$condition, type = "prob")
                            
                            #BGLM Model application ----------------------------
                            # Healthy
                            healthy <- model_predict(scene, models$healthy)
                            
                            # Wilted
                            wilted <- model_predict(scene, models$wilted)
                            
                            # Dead
                            dead <- model_predict(scene, models$dead)
                            
                            #Predicted scene
                            bglm_predicted <- c(healthy, wilted, dead)
                            names(bglm_predicted) <- c("healthy", "wilted", "dead")
                            
                            #Mask KNDVI
                            mask <- KNDVI[date[j]]
                            mask[mask < 5000] <- 0
                            mask[mask >= 5000] <- 1
                            bglm_predicted <- mask(bglm_predicted, mask, maskvalues = 0)
                            
                            export_name <- paste0(out_path, "/", 
                                                  sub_frame$tile[1], "/",
                                                  date[j], "_predicted.tif")
                            
                            #Export
                            writeRaster(x = bglm_predicted,
                                        filename = export_name,
                                        names = c("healthy", "wilted", "dead"),
                                        NAflag = 0,
                                        overwrite= TRUE)
                            
                            classes <- mask(classes, mask, maskvalues = 0)
                            
                            export_name <- paste0(out_path, "/", 
                                                  sub_frame$tile[1], "/",
                                                  date[j], "_predicted_LDA.tif")
                            
                            #Export
                            writeRaster(x = classes,
                                        filename = export_name,
                                        names = c("dead", "healthy", "wilted"),
                                        NAflag = 0,
                                        overwrite= TRUE)
                            
                          }
                          
                          #Remove residuals
                          #rm(list = c("scene", "healthy", "wilted", "dead",
                          #            "kNDVI", "predicted", "export_name"))
                          gc()
                          
                        }
  
  #Stop cluster
  stopCluster(cl)
  gc()
  
}
