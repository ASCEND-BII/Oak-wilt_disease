################################################################################
##### Estimation of trends from vegetation indices
################################################################################

#-------------------------------------------------------------------------------
# Source load

source("R/00-path_vi_scenes2.R")
source("R/00-fun_slope.R")

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(stars)
library(parallel)

#-------------------------------------------------------------------------------
#Arguments

#' @param root_path: select the root path of where the scenes are located
#' @param out_path: path of the outputs
#' @param range_doy: period in which trends will be estimated
#' @param threads: the number of threads to use for parallel processing

root_path <- "/media/antonio/antonio_ssd/FORCE/level4"
range_doy <- c(166, 258) #June 15 to August 31 258
out_path <- "/media/antonio/antonio_ssd/FORCE/level5"
threads <- 16

#-------------------------------------------------------------------------------
#Function
trends_vi <- function(root_path, out_path, range_doy, threads = 16) {
  
  #Get scenes to work with
  frame <- path_vi_scenes2(root_path)
  
  #Subset by doy
  frame <- subset(frame, doy >= range_doy[1] &
                    doy <= range_doy[2])
  
  #Unique set of observations
  frame[, unique := .GRP, by=.(tile, year)]
  
  #N set of observations
  n_scenes <- uniqueN(frame$unique)
  
  # Set up cluster
  #cl <- makeCluster(threads, type = "FORK")
  #registerDoParallel(cl)
  
  #Loop over tiles
  # Loop over scenes to estimate kNDVI
  #foreach(i = 1:n_scenes,
  #        .packages = c("stars"),
  #        .export = c("fun_slope"),
  #        .inorder = F) %dopar% {
  
  for(i in 1:n_scenes) {
           
            #Get set of observations
            sub_frame <- subset(frame, unique == i)
            
            #Files to read
            files <- paste0(root_path, "/", 
                            sub_frame$tile, "/", 
                            sub_frame$scene) 
            
            #Read scenes
            stars_files <- read_stars(files, 
                                      along = list(doy = sub_frame$doy))
            
            ### Trend---------------------------------------------------------------
            cls <- makeCluster(threads, type = "FORK")
            
            #Get slope
            trend <- st_apply(X = adrop(stars_files), 
                              MARGIN = c(1,2), 
                              FUN = fun_slope, 
                              doy = sub_frame$doy,
                              CLUSTER = cls)
            
            stopCluster(cls)
            gc()
            
            #Export name
            export_name <- paste0(out_path, "/", 
                                  sub_frame$tile[1], "/", 
                                  sub_frame$year[1], "_",
                                  sub_frame$VI[1], "_",
                                  "slope.tif")
            
            #Export
            write_stars(
              trend,
              dsn = export_name,
              layer = 1)
            
            #Remove residuals
            rm(list = c("sub_frame", "files", "stars_files", "trend", "export_name"))
            gc()
        
          }
  
  #Stop cluster
  #stopCluster(cl)
  #gc()
    
}

#' @example 
trends_vi(root_path, out_path, range_doy, threads = 26)
