################################################################################
##### Get path of the scenes for FORCE LEVEL 3
################################################################################

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(lubridate)

#-------------------------------------------------------------------------------
#Arguments

#' @param root_path select the root path of where the scenes are located
#root_path <- "/media/antonio/antonio_ssd/FORCE/level4"

#-------------------------------------------------------------------------------
#Functions

path_vi_scenes2 <- function(root_path) {
  
  #Search for paths
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
  
  #Get year, month, day, date, and doy
  frame[, year := substr(scene, 1, 4), by = seq_along(1:nrow(frame))]
  frame[, month := substr(scene, 6, 7), by = seq_along(1:nrow(frame))]
  frame[, day := substr(scene, 9, 10), by = seq_along(1:nrow(frame))]
  frame$date <- paste(frame$year, frame$month, frame$day, sep = "-")
  frame$date <- as.Date(frame$date)
  frame$doy <- yday(frame$date)
  
  #Get VI
  frame[, VI := substr(scene, 12, 16), by = seq_along(1:nrow(frame))]
  
  #Order 
  frame <- frame[order(VI, tile, date)]
  
  return(frame)
  
}