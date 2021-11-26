################################################################################
##### Get path of the scenes for FORCE LEVEL 3
################################################################################

#-------------------------------------------------------------------------------
# Libraries
#-------------------------------------------------------------------------------

library(data.table)

#-------------------------------------------------------------------------------
#Arguments
#-------------------------------------------------------------------------------
#root_path: select the root path of where the secenes are located
root_path <- "/media/antonio/Work/Oak-Sentinel/level3_sen2"

################################################################################
#Functions
path_scenes <- function(root_path) {
  
  #Search for paths
  files <- list.files(path = paste0(root_path), 
                      pattern = ".tif", 
                      all.files = TRUE,
                      full.names = FALSE, 
                      recursive = TRUE)
  
  #Arrange path in frame
  frame <- data.table(matrix(unlist(strsplit(files, "/")), nrow= length(files), byrow=TRUE), stringsAsFactors=FALSE)
  colnames(frame) <- c("tile", "scene")
  
  #Get date
  frame[, date := as.Date(strsplit(scene, "_")[[1]][8], "%Y%m%d"), 
        by = seq_along(1:nrow(frame))]
  
  #Get sensor
  frame[, sensor := substr(strsplit(scene, "_")[[1]][9], 1, 5), 
        by = seq_along(1:nrow(frame))]
  
  return(frame)
  
}
