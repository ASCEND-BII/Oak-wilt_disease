################################################################################
##### 03 - VI value extraction for time series
################################################################################

#' @description Batch extraction of values of VI using a vector file 
#' for training.

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(terra)
library(doParallel)
library(foreach)

#-------------------------------------------------------------------------------
#Arguments

#' @param root_path: select the root path of where the scenes are located
#' @param vector_path: path of the ..gpkg file
#' @param out_path: path and name of the .txt outputs
#' @param threads: the number of threads to use for parallel processing

root_path <- "F:/FORCE/level3_shifted/X0014_Y0024"
vector_path <- "F:/FORCE/level3_shifted/training_X0014_0024.gpkg"
out_path <- "F:/FORCE/level3_shifted/X0014_0024_VI.txt"
threads <- 10

#-------------------------------------------------------------------------------
#Arguments

vi_extraction <- function(root_path, vector_path, threads = 26) {
  
  #Load vector
  vector <- vect(vector_path)
  
  #Search for VI paths
  files <- list.files(path = root_path, 
                      pattern = ".tif", 
                      all.files = TRUE,
                      full.names = FALSE, 
                      recursive = TRUE)
  
  #Arrange path in frame
  frame <- data.table(matrix(unlist(strsplit(files, "/")), 
                             nrow= length(files), 
                             byrow=TRUE), stringsAsFactors=FALSE)
  colnames(frame) <- c("scene")
  
  #Get year, month, day, date, and doy
  frame[, date2 := strsplit(scene, "_")[[1]][8], by = seq_along(1:nrow(frame))]
  frame[, year := substr(date2, 1, 4), by = seq_along(1:nrow(frame))]
  frame[, month := substr(date2, 5, 6), by = seq_along(1:nrow(frame))]
  frame[, day := substr(date2, 7, 8), by = seq_along(1:nrow(frame))]
  frame$date <- paste(frame$year, frame$month, frame$day, sep = "-")
  frame$date <- as.Date(frame$date)
  frame$doy <- yday(frame$date)
  
  #Frame
  frame <- frame[, c(1, 3, 4, 5, 6, 7)]
  
  #Get VI
  frame[, VI := strsplit(scene, "_")[[1]][6], by = seq_along(1:nrow(frame))]
  
  #Order 
  frame <- frame[order(VI, date)]
  
  #Unique date and tile
  frame[, unique := .GRP, by=.(date)]
  
  #N scenes
  n_scenes <- uniqueN(frame$unique)
  
  # Set up cluster
  cl <- makeCluster(threads, type = "FORK")
  registerDoParallel(cl)
  
  # Loop over scenes to estimate kNDVI
  extraction <- foreach(i = 1:n_scenes,
                        .combine = rbind,
                        .packages = c("terra", "data.table"),
                        .inorder = F) %dopar% {
                          
                          #Files
                          CCI <- frame[VI == "CCI" & unique == i]
                          CRE <- frame[VI == "CRE" & unique == i]
                          NDM <- frame[VI == "NDM" & unique == i]
                          NDV <- frame[VI == "NDV" & unique == i]
                          
                          #Read rasters
                          rCCI <- rast(paste0(root_path, "/", CCI$scene[1]))
                          rCRE <- rast(paste0(root_path, "/", CRE$scene[1]))
                          rNDM <- rast(paste0(root_path, "/", NDM$scene[1]))
                          rNDV <- rast(paste0(root_path, "/", NDV$scene[1]))
                          
                          #Merge rasters
                          VI <- c(rCRE, rNDM, rCCI, rNDV)
                          names(VI) <- c("CRE", "NDM", "CCI", "NDV")
                          
                          #Extract values
                          values <- extract(VI, 
                                            vector, 
                                            method = "simple",
                                            factors = TRUE,
                                            cells = FALSE,
                                            xy = TRUE,
                                            exact = TRUE,
                                            weights = TRUE, 
                                            touches = TRUE)
                          
                          #Add date
                          values$date <- CCI$date[1]
                          
                          #Return
                          return(values)
                          
                        }
  
  #Stop cluster
  stopCluster(cl)
  gc()
  
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
  
}

#' @example 
root_path <- "F:/FORCE/level3_VI/X0015_Y0024"
vector_path <- "F:/FORCE/level3_shifted/training_X0015_0024.gpkg"
out_path <- "F:/FORCE/level3_shifted/X0015_0024_VI.txt"
threads <- 10

vi_extraction(root_path, out_path, threads)