################################################################################
####### Queue files for FORCE
################################################################################

#-------------------------------------------------------------------------------
# Libraries

library(data.table)

#-------------------------------------------------------------------------------
#Arguments

#path: path where the satellite imagery is located
path <- "/media/antonio/Files/MN-Sentinel-Landsat/Landsat"

#pattern: Pattern for search. It should be ".SAFE" for Sentinel-2 or
#         ".tar" for Landsat-8
pattern <- ".tar"

#out_put: name of the .txt queue file
out_put <- "queue_landsat.txt"

#-------------------------------------------------------------------------------
# Function
get_queued_files <- function(path, pattern, out_put) {
  
  path_elements <- length(strsplit(path, split = "/")[[1]])+1
  
  sensor <- match.arg(pattern, c(".tar", ".SAFE"))
  
  #Get scenes
  files <- list.files(path = path, 
                      pattern = sensor, 
                      all.files = TRUE,
                      full.names = TRUE, 
                      recursive = TRUE,
                      include.dirs = TRUE)
  
  frame <- data.table(files = files, QUEUED = "QUEUED")
  
  if(sensor == ".tar") {
    
    frame[ , date := as.Date(paste0(substr(strsplit(files, split = "/")[[1]][path_elements], 58, 61), "-",
                                    substr(strsplit(files, split = "/")[[1]][path_elements], 62, 63), "-",
                                    substr(strsplit(files, split = "/")[[1]][path_elements], 64, 65))),
           by = seq_len(nrow(frame))]
    
    
    frame[ , tile := substr(strsplit(files, split = "/")[[1]][path_elements], 11, 16),
          by = seq_len(nrow(frame))]
    
    frame <- frame[order(date, tile)]
    
  } else if(sensor == ".SAFE") {
    
    frame[ , date := as.Date(paste0(substr(strsplit(files, split = "/")[[1]][path_elements], 12, 15), "-",
                                    substr(strsplit(files, split = "/")[[1]][path_elements], 16, 17), "-",
                                    substr(strsplit(files, split = "/")[[1]][path_elements], 18, 19))),
           by = seq_len(nrow(frame))]
    
    frame <- frame[order(date)]
    
    
  }
  
  fwrite(frame[, 1:2], out_put, sep = " ")
  
}


