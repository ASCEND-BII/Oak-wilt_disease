################################################################################
##### 03 - VI value extraction for bi-temporal observations
################################################################################

#' @description Batch extraction of values of VI using a vector file 
#' for training.

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(terra)
library(foreach)

#-------------------------------------------------------------------------------
#Arguments

#' @param root_path: select the root path of where the scenes are located
#' @param vector_path: path of the ..gpkg file
#' @param out_path: path and name of the .txt outputs
#' @param threads: the number of threads to use for parallel processing

root_path <- "F:/FORCE/level4/X0014_Y0024"
vector_path <- "F:/FORCE/level3_shifted/training_X0014_0024.gpkg"
out_path <- "F:/FORCE/level4/X0014_0024_dVI.txt"

#-------------------------------------------------------------------------------
#Arguments

dvi_extraction <- function(root_path, vector_path, out_path) {
  
  #Load vector
  vector <- vect(vector_path)
  
  #Search for VI paths
  files <- list.files(path = root_path, 
                      pattern = ".tif", 
                      all.files = TRUE,
                      full.names = FALSE, 
                      recursive = TRUE)
  
  #Arrange path in frame
  frame <- data.table(matrix(unlist(strsplit(files, "_")), 
                             nrow= length(files), 
                             byrow=TRUE), stringsAsFactors=FALSE)
  colnames(frame)[1] <- c("year")
  
  #Frame
  frame <- frame[, c(1)]
  
  #N scenes
  n_scenes <- uniqueN(frame$year)
  
  # Loop over scenes to estimate kNDVI
  extraction <- foreach(i = 1:n_scenes,
                        .combine = rbind,
                        .packages = c("terra", "data.table"),
                        .inorder = F) %do% {
                          
                          #File
                          file <- paste0(root_path, "/", files[i])
                          scene <- rast(file)
                          
                          #Extract values
                          values <- extract(scene, 
                                            vector, 
                                            method = "simple",
                                            factors = TRUE,
                                            cells = FALSE,
                                            xy = TRUE,
                                            exact = TRUE,
                                            weights = TRUE, 
                                            touches = TRUE)
                          
                          #Add date
                          values$date <- frame$year[i]
                          
                          #Return
                          return(values)
                          
                        }
  
  #Prepare vector to merge
  area <- expanse(vector)
  vector <- as.data.frame(vector)
  vector$ID <- 1:nrow(vector)
  vector <- as.data.table(vector)
  vector$area <- area
  
  #Merge
  complete <- merge(vector, extraction, by = "ID", all.x = TRUE, all.y = TRUE)
  
  #Add names
  complete[Condition == "1", condition := "healthy"]
  complete[Condition == "2", condition := "wilted"]
  complete[Condition == "3", condition := "dead"]
  
  #Order complete
  complete <- complete[, c(1, 12, 3, 11, 8, 9, 10, 4, 5, 6, 7)]
  colnames(complete)[5:7] <- c("x", "y", "weight")
  
  #Export
  fwrite(complete, out_path, sep = "\t")
  
  #Return
  return(complete)
  
}

#' @example 
root_path <- "F:/FORCE/level4/X0015_Y0024"
vector_path <- "F:/FORCE/level3_shifted/training_X0015_0024.gpkg"
out_path <- "F:/FORCE/level4/X0015_0024_dVI.txt"

dvi_extraction(root_path, vector_path, out_path)
