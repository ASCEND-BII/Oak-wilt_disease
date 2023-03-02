################################################################################
##### 04 - Predict models (MSI function)
################################################################################

#' @description Batch for prediction of the models using the LSP scenes.

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(terra)
library(Rcpp)
library(RcppArmadillo)
library(parallel)

#-------------------------------------------------------------------------------
# Source code

sourceCpp("/panfs/jay/groups/17/cavender/shared/oak-wilt/misc/R/Oak-wilt/src/model-application.cpp")

#-------------------------------------------------------------------------------
# Root path
#path <- "/media/antonio/Work/Projects/Oak-wilt_mapping"

#-------------------------------------------------------------------------------
#Arguments

#' @param root_path Select the root path of where the scenes are located.
#' @param healthy A data.table of coefficients for healthy condition.
#' @param symtomatic A data.table of coefficients for symtomatic condition.
#' @param dead A data.table of coefficients for dead condition.
#' @param years Intergers with the years to evaluate.
#' @param out_path path and name of the .txt outputs
#' @param threads the number of threads to use for parallel processing

#-------------------------------------------------------------------------------
#Function

apply_model <- function(root_path, 
                        healthy, 
                        symtomatic,
                        dead,
                        threads) {
  
  #--------------------------------------------------------
  # Selection and arrangement of files
  
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
  frame[, metric := substr(strsplit(scene, "_")[[1]][7], 1, 3), 
        by = seq_along(1:nrow(frame))]
  
  frame <- frame[order(tile, metric)]
  
  #Subset metric
  frame <- subset(frame, metric == "VSS" |
                    metric == "VES" |
                    metric == "VGM" |
                    metric == "VGV")
  
  #Get years
  eval_years <- paste0("YEAR-", years)
  
  #Get unique tiles
  unique_tile <- unique(frame$tile)
  
  #--------------------------------------------------------
  # Create folders to dump the results
  for(i in 1:length(unique_tile)) {
    
    dir_name <- paste0(out_path, "/", unique_tile[i])
    
    if(file.exists(dir_name) != TRUE) {
      dir.create(dir_name)
    }
  }
  
  #--------------------------------------------------------
  # Makes coefficients matrices
  
  coef_healthy <- as.matrix(healthy)
  coef_symtomatic <- as.matrix(symtomatic)
  coef_dead <- as.matrix(dead)
  
  #--------------------------------------------------------
  #Function for batch application
  application <- function(i, 
                          frame, 
                          coef_healthy,
                          coef_symtomatic,
                          coef_dead,
                          eval_years) {
    
    sub_frame <- frame[tile == unique_tile[i]]
    
    #Read scenes ------------------------------------------
    VSS <- sub_frame[metric == "VSS" & VI == "CCI"]
    VSS <- rast(paste0(root_path, "/",
                       VSS$tile[1], "/", 
                       VSS$scene[1]))
    
    VES <- sub_frame[metric == "VES" & VI == "CCI"]
    VES <- rast(paste0(root_path, "/",
                       VES$tile[1], "/", 
                       VES$scene[1]))
    
    VGM <- sub_frame[metric == "VGM" & VI == "CCI"]
    VGM <- rast(paste0(root_path, "/",
                       VGM$tile[1], "/", 
                       VGM$scene[1]))
    
    VGV <- sub_frame[metric == "VGV" & VI == "CCI"]
    VGV <- rast(paste0(root_path, "/",
                       VGV$tile[1], "/", 
                       VGV$scene[1]))
    
    #Loop over years
    for(j in 1:length(eval_years)) {
      
      #Prepare scenes ---------------------------------------
      #Mask scene
      mask <- VSS[eval_years[j]]
      mask[mask < 2500] <- 0
      mask[mask >= 2500] <- 1
      
      #Get VCV
      VCV <- VGV[eval_years[j]]/VGM[eval_years[j]]
      VCV[VCV == Inf] <- NA
      
      #Get values per mask
      vss <- mask(VSS[eval_years[j]], mask, maskvalues = 0)
      ves <- mask(VES[eval_years[j]], mask, maskvalues = 0)
      vcv <- mask(VCV, mask, maskvalues = 0)
      
      #Get normalization values
      vss_mean <- mean(vss[], na.rm = TRUE)
      ves_mean <- mean(ves[], na.rm = TRUE)
      vcv_mean <- mean(vcv[], na.rm = TRUE)
      
      vss_sd <- sd(vss[], na.rm = TRUE)
      ves_sd <- sd(ves[], na.rm = TRUE)
      vcv_sd <- sd(vcv[], na.rm = TRUE)
      
      #Apply z-score normalization
      vss <- (VSS[eval_years[j]] - vss_mean)/vss_sd
      ves <- (VES[eval_years[j]] - ves_mean)/ves_sd
      vcv <- (VCV - vcv_mean)/vcv_sd
      
      #Change names
      names(vss) <- "VSS"
      names(ves) <- "VES"
      names(vcv) <- "VCV"
      
      #Stack scene
      scene <- c(vss,
                 ves,
                 vcv)
      
      #Transform to frame and get any NA
      scene_frame <- as.data.table(as.data.frame(scene, cells = TRUE))
      nas <- apply(scene_frame, 1, anyNA)
      scene_frame[nas == TRUE, VSS := NA]
      scene_frame[nas == TRUE, VES := NA]
      scene_frame[nas == TRUE, VCV := NA]
      
      #Apply models ----------------------------------------
      #Apply models and get uncertainties
      predictions <- predicted_prob(as.matrix(scene_frame[nas != TRUE, c("VSS", "VES", "VCV")]), 
                                    coef_healthy,
                                    coef_symtomatic,
                                    coef_dead)
      
      predictions <- as.data.table(predictions)
      colnames(predictions) <- c("Healthy", "Symtomatic", "Dead", 
                                 "amp-healthy", "amp-symtomatic", "amp-dead")
      predictions$cell <- scene_frame[nas != TRUE, cell]
      
      #Merge predictions and scene_frame
      scene_frame <- merge(scene_frame, predictions, by = "cell", all.x = TRUE)
      scene_frame <- scene_frame[, c(5:10)]
      
      scene_predictions <- c(vss, vss, vss, vss, vss, vss)
      scene_predictions[] <- as.matrix(scene_frame)
      
      #Second mask -------------------------------------
      mask <- VSS[eval_years[j]]
      mask[mask < 600] <- 0
      mask[mask >= 600] <- 1
      
      scene_predictions <- mask(scene_predictions, mask, maskvalues = 0)
      
      #Export name
      export_predicted <- paste0(out_path, "/", 
                                 sub_frame$tile[1], "/",
                                 eval_years[j], "_predicted.tif")
      
      #Export
      writeRaster(x = scene_predictions,
                  filename = export_predicted,
                  names = c("Healthy", "Symtomatic", "Dead", 
                            "amp-healthy", "amp-symtomatic", "amp-dead"),
                  NAflag = -9999,
                  overwrite= TRUE,
                  datatype = "INT4S")
      
      #Remove residuals
      rm(list = c("mask", "VCV", 
                  "vss", "ves", "vcv",
                  "vss_mean", "ves_mean", "vcv_mean",
                  "vss_sd", "ves_sd", "vcv_sd",
                  "scene_frame", "nas","predictions",
                  "export_predicted", "scene_predictions"))
      
    }
    
    #Remove residuals
    rm(list = c("sub_frame", "VSS", 
                "VGM", "VGV", "VES"))
    gc()
    
    return(NA)
    
  }
  
  # Parallel
  mclapply(1:length(unique_tile), 
           FUN = application, 
           frame = frame, 
           coef_healthy,
           coef_symtomatic,
           coef_dead,
           eval_years,
           mc.cores = 16,
           mc.preschedule = TRUE,
           mc.cleanup = TRUE)
  
}

#-------------------------------------------------------------------------------
#' @example  

root_path <- "/panfs/jay/groups/17/cavender/shared/oak-wilt/level3"
healthy <- fread("/panfs/jay/groups/17/cavender/shared/oak-wilt/misc/R/Oak-wilt/data/coef-healthy.csv")
symtomatic <- fread("/panfs/jay/groups/17/cavender/shared/oak-wilt/misc/R/Oak-wilt/data/coef-symtomatic.csv")
dead <- fread("/panfs/jay/groups/17/cavender/shared/oak-wilt/misc/R/Oak-wilt/data/coef-dead.csv")
years <- 2017:2022
out_path <- "/panfs/jay/groups/17/cavender/shared/oak-wilt/level4"
threads <- 64

apply_model(root_path, 
            healthy, 
            symtomatic,
            dead,
            threads)
