################################################################################
##### Function to get the trend from a set of scenes
################################################################################

#-------------------------------------------------------------------------------
# Source load

source("R/fun_slope.R")

#-------------------------------------------------------------------------------
# Libraries
library(raster)
library(lubridate)

#-------------------------------------------------------------------------------
#Arguments

#scenes: a vector that describe the scenes with their path
#date: date of each scene ("%Y%m%d"), both scenes and date must match
#threads: select the number of threads to use for parallel computing

#-------------------------------------------------------------------------------
#Function

get_trend <- function(scenes, date, threads = 12) {
  
  #Get julian day
  julian_day <- yday(date)
  
  #Stack scenes
  raster_stack <- stack(scenes)
  
  #Start cluster
  beginCluster(threads)
  
  #Get trend
  slope <- clusterR(x = raster_stack, 
                    fun = calc, 
                    args = list(fun_slope),
                    export= 'julian_day')
  #End cluster
  endCluster()
  
}