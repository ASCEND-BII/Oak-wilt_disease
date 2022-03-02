################################################################################
##### Estimation of trends from vegetation indices
################################################################################

#-------------------------------------------------------------------------------
# Source load

source("R/00-path_vi_scenes.R")
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
#' @param vi: select the vi of interest
#' @param range_doy: period in which trends will be estimated
#' @param overwrite if the file exist, do you want to create it again?
#' @param threads: the number of threads to use for parallel processing

root_path <- "/media/antonio/antonio_ssd/level3"
range_doy <- c(166, 258) #June 15 to August 31 258
out_path <- "/media/antonio/antonio_ssd/level4"
threads <- 26

#-------------------------------------------------------------------------------
#Function
trends_vi <- function(root_path, out_path, vi = "N1N", range_doy, overwrite = TRUE, threads = 16) {
  
  #Get scenes to work with
  frame <- path_vi_scenes(root_path)
  
  #Unique tiles
  unique_tile <- unique(frame$tile)
  
  #Subset iv
  frame <- subset(frame, VI == vi)
  
  #Subset by doy
  frame <- subset(frame, doy >= range_doy[1] &
                    doy <= range_doy[2])
  
  #Progress bar
  pb <- txtProgressBar(min = 1, 
                       max = length(unique_tile), 
                       style = 3)
  n <- 1
  
  #Loop over tiles
  for(i in 1:length(unique_tile)) {
    
    #Progress
    setTxtProgressBar(pb, n)
    n <- n+1
    
    #Subset to work with a given tile
    sub_tile <- subset(frame, tile == unique_tile[i])
    
    #Unique years
    unique_years <- unique(sub_tile$year)
    
    #Loop over years
    #Start at 2 to exclude 2017
    for(ii in 2:length(unique_years)) { 
      
      #Subset layers for a given year
      sub_year <- subset(sub_tile, year(date) == unique_years[ii])
      
      if(nrow(sub_year) >= 3) {
        
        ### Read start----------------------------------------------------------
        
        sub_files <- paste0(root_path, "/", 
                            sub_year$tile, "/", 
                            sub_year$scene) 
        
        stars_files <- read_stars(sub_files, 
                                  along = list(doy = sub_year$doy))
        
        ### Trend---------------------------------------------------------------
        cls <- makeCluster(threads, type = "FORK")
        
        trend <- st_apply(X = adrop(stars_files), 
                          MARGIN = c(1,2), 
                          FUN = fun_slope, 
                          doy = sub_year$doy,
                          CLUSTER = cls)
        stopCluster(cls)
        gc()
        
        ### Export -------------------------------------------------------------
        export_name <- paste0(out_path, "/", 
                              sub_year$tile[1], "/", 
                              sub_year$year[1], "_",
                              sub_year$VI[1], "_",
                              "trend.tif")
        
        write_stars(
          trend,
          dsn = export_name,
          layer = 1)
        
      } 
    }
  }
}

#' @example 
trends_vi(root_path, out_path, vi = "N1N", range_doy, overwrite = TRUE, threads = 26)
