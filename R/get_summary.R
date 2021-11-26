################################################################################
##### Function to get the summary set of scenes
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

get_summary <- function(scenes, date) {
  
  #Get trend
  slope <- app(scenes, 
               fun = fun_slope,
               date = doy,
               cores = threads)
  
}