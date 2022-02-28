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

#-------------------------------------------------------------------------------
#Arguments

#' @param root_path: select the root path of where the scenes are located
#' @param out_path: path of the outputs
#' @param vi: select the vi of interest
#' @param range_doy: period in which trends will be estimated
#' @param overwrite if the file exist, do you want to create it again?
#' @param threads: the number of threads to use for parallel processing

root_path <- "/media/antonio/antonio_ssd/level3"
range_doy <- c(121, 243) #May to August
out_path <- "/media/antonio/antonio_ssd/level4"
threads <- 4

#-------------------------------------------------------------------------------
#Function
trends_sen2 <- function(root_path, out_path, vi = "EVI", range_doy, overwrite = FALSE, threads) {
  
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
                       max = length(unique_tile)*length(unique_years), 
                       style = 3)
  n <- 1
  
  #Loop over tiles
  for(i in 1:length(unique_tile)) {
    
    #Subset to work with a given tile
    sub_tile <- subset(frame, tile == unique_tile[i])
    
    #Unique years
    unique_years <- unique(sub_tile$year)
    
    #Loop over years
    for(ii in 1:length(unique_years)) { 
      
      #Progress
      setTxtProgressBar(pb, n)
      n <- n+1
      
      #Subset layers for a given year
      sub_year <- subset(sub_tile, year(date) == unique_years[ii])
      
      if(nrow(sub_year) >= 3) {
        
        ###Read start-----------------------------------------------------------
        
        sub_files <- paste0(root_path, "/", 
                            sub_year$tile, "/", 
                            sub_year$scene) 
        
        stars_files <- read_stars(sub_files, 
                                  along = list(doy = sub_year$doy))
        
        ###Trend----------------------------------------------------------------
        trend <- st_apply(adrop(stars_files), c(1,2), fun_slope, doy = sub_year$doy)
        
        plot(trend, z = c(-1, 1) breaks = "equal", main = "9-day time trend (slope)")
      } 
    }
  }
}

#' @example 
trends_sen2(root_path, evaluation_doy, band_select, mask_path, out_path, overwrite = FALSE, threads)

write_stars(
  trend,
  dsn = "test.tif",
  layer = 1)

