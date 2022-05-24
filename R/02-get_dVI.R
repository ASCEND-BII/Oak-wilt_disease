################################################################################
##### Estimation of delta observations of VI
################################################################################

#-------------------------------------------------------------------------------
# Source load

source("R/00-path_vi_scenes.R")

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(terra)
library(foreach)
library(doParallel)

#-------------------------------------------------------------------------------
#Arguments

#' @param root_path: select the root path of where the scenes are located
#' @param out_path: path of the outputs
#' @param range_doy1: time 1 range
#' @param range_doy2: time 2 range
#' @param threads: the number of threads to use for parallel processing

#root_path <- "F:/FORCE/level3"
#range_doy1 <- c(135, 166)
#range_doy2 <- c(222, 232) #August 10 to 24
#out_path <- "F:/FORCE/level4"
#threshold <- 5000
#threads <- 6

#-------------------------------------------------------------------------------
#Function
get_dVI <- function(root_path, out_path, 
                         range_doy1, range_doy2, 
                         threshold = 5000, threads = 5) {
  
  #Get scenes to work with
  frame <- path_vi_scenes(root_path)
  
  #Subset by doy
  frame_doy1 <- subset(frame, doy >= range_doy1[1] & doy <= range_doy1[2])
  frame_doy1$time <- 1
  frame_doy2 <- subset(frame, doy >= range_doy2[1] & doy <= range_doy2[2])
  frame_doy2$time <- 2
  frame <- rbind(frame_doy1, frame_doy2)
  frame <- frame[order(tile, date)]
  
  #VI of interest
  frame <- subset(frame, VI == "CCI" | VI == "CRE" | VI == "NDW" | VI == "NDV")
  
  #Unique set of observations
  frame[, unique := .GRP, by = .(tile, year)]
  
  #N set of observations
  n_scenes <- uniqueN(frame$unique)
  
  #Matrix of weights
  wmatrix <- matrix(c(0, -1, 0, -1, 5, -1, 0, -1, 0), nc=3, nr=3)
  
  #Set up cluster
  cl <- makeCluster(threads, type = "FORK")
  registerDoParallel(cl)
  
  # Loop over scenes to estimate delta VI
  foreach(i = 1:n_scenes,
          .packages = c("terra", "data.table"),
          .inorder = F) %dopar% {
  
    #Get set of observations
    sub_frame <- subset(frame, unique == i)
    time1 <- subset(sub_frame, time == 1)
    time2 <- subset(sub_frame, time == 2)
    
    #logic to skip
    n_times <- uniqueN(sub_frame$time)
    
    #Finish if there is not different periods
    if(n_times == 2) {
      
      #CCI -------------------------------------------
      CCI_1 <- rast(paste0(root_path, "/",
                           time1$tile[1], "/",
                           time1[VI == "CCI", scene]))
      CCI_1 <- mean(CCI_1, na.rm = T)
      
      CCI_2 <- rast(paste0(root_path, "/",
                           time2$tile[1], "/",
                           time2[VI == "CCI", scene]))
      CCI_2 <- mean(CCI_2, na.rm = T)
      
      delta_CCI <- round(CCI_2 - CCI_1)
      
      #Remove inf
      inf <- is.infinite(values(delta_CCI))
      values(delta_CCI)[inf == TRUE] <- NA
      
      #NDVW ------------------------------------------
      NDW_1 <- rast(paste0(root_path, "/",
                           time1$tile[1], "/",
                           time1[VI == "NDW", scene]))
      NDW_1 <- mean(NDW_1, na.rm = T)
      
      NDW_2 <- rast(paste0(root_path, "/",
                           time2$tile[1], "/",
                           time2[VI == "NDW", scene]))
      NDW_2 <- mean(NDW_2, na.rm = T)
      
      delta_NDW <- round(NDW_2 - NDW_1)
      
      #Remove inf
      inf <- is.infinite(values(delta_NDW))
      values(delta_NDW)[inf == TRUE] <- NA
      
      #CRE -------------------------------------------
      CRE_1 <- rast(paste0(root_path, "/",
                           time1$tile[1], "/",
                           time1[VI == "CRE", scene]))
      CRE_1 <- mean(CRE_1, na.rm = T)
      
      CRE_2 <- rast(paste0(root_path, "/",
                           time2$tile[1], "/",
                           time2[VI == "CRE", scene]))
      CRE_2 <- mean(CRE_2, na.rm = T)
      
      delta_CRE <- round(CRE_2 - CRE_1)
      
      #Remove inf
      inf <- is.infinite(values(delta_CRE))
      values(delta_CRE)[inf == TRUE] <- NA
      
      #Focal observations
      fCCI <- focal(delta_CCI, 
                    w = wmatrix,
                    fun = "sum",
                    na.rm = TRUE)
      
      fNDW <- focal(delta_NDW, 
                    w = wmatrix,
                    fun = "sum",
                    na.rm = TRUE)
      
      fCRE <- focal(delta_CRE, 
                    w = wmatrix,
                    fun = "sum",
                    na.rm = TRUE)
      
      # Mask application -----------------------------
      # Read NDVI
      NDVI <- rast(paste0(root_path, "/",
                          time1$tile[1], "/",
                          time1[VI == "NDV", scene]))
      
      #Get mean
      NDVI <- mean(NDVI, na.rm = T)
      
      #Look for threshold
      mask <- values(NDVI) >= threshold
      values(NDVI)[mask == TRUE] <- 1
      values(NDVI)[mask != TRUE] <- 0
      
      #Apply mask
      dCCI <- mask(fCCI, NDVI, maskvalues = 0)
      dNDW <- mask(fNDW, NDVI, maskvalues = 0)
      dCRE <- mask(fCRE, NDVI, maskvalues = 0)
      
      #Export ----------------------------------------
      dVI <- c(dCCI, dNDW, dCRE)
      
      export_name <- paste0(out_path, "/", 
                            sub_frame$tile[1], "/",
                            sub_frame$year[1], "_dVI.tif")
      
      #Export
      writeRaster(x = dVI,
                  filename = export_name,
                  names = c("dCCI", "dNDW", "dCRE"),
                  NAflag = NA,
                  overwrite=TRUE)
      
      #Remove residuals
      rm(list = c("sub_frame", "time1", "time2", "n_times", "inf",
                  "NDVI", "mask", "export_name",
                  "CCI_1", "CCI_2", "delta_CCI", "dCCI", "fCCI",
                  "NDW_1", "NDW_2", "delta_NDW", "dNDW", "fNDW",
                  "CRE_1", "CRE_2", "delta_CRE", "dCRE", "fCRE"))
      gc()
      
      }
    }

  #Stop cluster
  stopCluster(cl)
  gc()
  
}

#' @example 
root_path <- "/home/cavender/shared/oak-wilt/level3"
range_doy1 <- c(135, 166)
range_doy2 <- c(222, 232)
out_path <- "/home/cavender/shared/oak-wilt/level4"
threshold <- 5000
threads <- 6
get_dVI(root_path, out_path, range_doy1, range_doy2, threshold, threads)
