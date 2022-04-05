################################################################################
##### Estimation of trends from vegetation indices
################################################################################

#-------------------------------------------------------------------------------
# Source load

source("R/00-path_vi_scenes2.R")

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

root_path <- "/media/antonio/antonio_ssd/FORCE/level4"
range_doy1 <- c(152, 181) #June 1 to 15
range_doy2 <- c(222, 232) #August 15 to 31
out_path <- "/media/antonio/antonio_ssd/FORCE/level5"
threads <- 28

#-------------------------------------------------------------------------------
#Function
trends_vi <- function(root_path, out_path, range_doy1, range_doy2, threads = 16) {
  
  #Get scenes to work with
  frame <- path_vi_scenes2(root_path)
  
  #Subset by doy
  frame_doy1 <- subset(frame, doy >= range_doy1[1] & doy <= range_doy1[2])
  frame_doy1$time <- 1
  frame_doy2 <- subset(frame, doy >= range_doy2[1] & doy <= range_doy2[2])
  frame_doy2$time <- 2
  frame <- rbind(frame_doy1, frame_doy2)
  frame <- frame[order(tile, date)]
  
  #Unique set of observations
  frame[, unique := .GRP, by=.(tile, year)]
  
  #N set of observations
  n_scenes <- uniqueN(frame$unique)
  
  #Matrix of weights
  wmatrix <- -matrix(1, nc=3, nr=3)
  
  #Set up cluster
  cl <- makeCluster(threads, type = "FORK")
  registerDoParallel(cl)
  
  
  # Loop over scenes to estimate kNDVI
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
    if(n_times == 1) {
      next
    }
    
    #Get rasters
    rtime1 <- rast(paste0(root_path, "/", 
                          time1$tile, "/", 
                          time1$scene))
    
    rtime2 <- rast(paste0(root_path, "/", 
                         time2$tile, "/", 
                         time2$scene))
    
    #Get mean
    mean_time1 <- mean(rtime1, na.rm = T)
    mean_time2 <- mean(rtime2, na.rm = T)
    
    #Subtraction
    delta_NGRDI <- round(mean_time2 - mean_time1)
    
    #Kernel application
    cov_dNRGI <- focal(delta_NGRDI,
                       w = wmatrix,
                       fun = sum)
    cov_dNRGI <- -(delta_NGRDI*8)/cov_dNRGI
    
    
    #Export name
    export_name <- paste0(out_path, "/", 
                          sub_frame$tile[1], "/",
                          sub_frame$year[1], 
                          "new_conv_NGRDI.tif")
    
    #Export
    writeRaster(x = cov_dNRGI,
                filename = export_name,
                names = sub_frame$year[1],
                datatype = "INT16S",
                NAflag = -999999)
    
    #Remove residuals
    rm(list = c("sub_frame", "files", "stars_files", "trend", "export_name"))
    gc()
    
    }

  #Stop cluster
  stopCluster(cl)
  gc()
  
}

#' @example 
trends_vi(root_path, out_path, range_doy, threads = 26)
