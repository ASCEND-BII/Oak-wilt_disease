################################################################################
##### 03 - Extraction for pixels from L3 products
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

path <- "F:/TRAINING"

root_path <- paste0(path, "/level3/X0014_Y0024")
vector_path <- paste0(path, "/AVIRIS-NG/X0014_Y0024_aviris.gpkg")
out_path <- paste0(path, "/level3_ts-pixels/X0014_Y0024_AVIRIS_ts.txt")
vi_extraction(root_path, vector_path, out_path)

#-------------------------------------------------------------------------------
#Arguments

vi_extraction <- function(root_path, vector_path, out_path) {
  
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
  
  #Get VI
  frame[, VI := strsplit(scene, "_")[[1]][6], by = seq_along(1:nrow(frame))]
  
  #Get method
  frame[, metric := substr(strsplit(scene, "_")[[1]][7], 1, 3), by = seq_along(1:nrow(frame))]
  
  #Order 
  frame <- frame[order(VI, metric)]
  
  #N scenes
  n_scenes <- nrow(frame)
  
  #Collect results
  extraction <- data.table()
  
  for(i in 1:n_scenes) {
    
    #Read
    VI <- rast(paste0(root_path, "/", frame$scene[i]))
    
    #Extract values
    values <- extract(VI, 
                      vector, 
                      method = "simple",
                      xy = TRUE)
    
    values_melted <- melt(as.data.table(values), 
                          id.vars = c("ID", "x", "y"), 
                          variable.name = "date",
                          value.name = "value")
    
    values_melted$VI <- frame$VI[i]
    values_melted$metric <- frame$metric[i]
    
    #Remove residuals
    rm(list = c("VI", "values"))
    gc()
    
    #Return
    extraction <- rbind(extraction, values_melted)
    
  }
  
  #Change names
  colnames(extraction) <- c("ID", "x", "y", "date", "value", "VI", "metric")
  
  #Prepare vector to merge
  area <- expanse(vector)
  vector <- as.data.frame(vector)
  vector$ID <- 1:nrow(vector)
  vector <- as.data.table(vector)
  #vector$area <- area
  
  #Merge
  complete <- merge(vector, extraction, by = "ID", all.x = TRUE, all.y = TRUE)
  
  #Add names
  complete[condition == "1", Condition := "healthy"]
  complete[condition == "2", Condition := "wilted"]
  complete[condition == "3", Condition := "dead"]
  
  #Order complete
  #complete <- complete[, c(1, 9, 3:4, 7, 8, 5, 6)]
  complete <- complete[, c(1, 10, 4:5, 8, 9, 6, 7)]

  #Modify year
  #complete$year <- as.numeric(substr(complete$year, 6, 9))
  #complete <- complete[year != 2016]
  complete$date <- as.character(complete$date)
  complete$date <- as.Date(paste0(substr(complete$date, 1, 4), "-",
                                  substr(complete$date, 5, 6), "-",
                                  substr(complete$date, 7, 8)))
  
  #complete[method == "TSI.tif", method := "TSI"]
  #complete[method == "TSS.tif", method := "TSS"]
  
  #Export
  fwrite(complete, out_path, sep = "\t")
  
}

#' @example 
root_path <- paste0(path, "/level3/X0014_Y0024")
vector_path <- paste0(path, "/AVIRIS-NG/X0014_Y0024_aviris.gpkg")
out_path <- paste0(path, "/level3_ts-pixels/X0014_Y0024_AVIRIS_ts.txt")
vi_extraction(root_path, vector_path, out_path)

root_path <- paste0(path, "/level3/X0015_Y0024")
vector_path <- paste0(path, "/AVIRIS-NG/X0015_Y0024_aviris.gpkg")
out_path <- paste0(path, "/level3_ts-pixels/X0015_Y0024_AVIRIS_ts.txt")
vi_extraction(root_path, vector_path, out_path)

root_path <- paste0(path, "/level3/X0016_Y0024")
vector_path <- paste0(path, "/AVIRIS-NG/X0016_Y0024_aviris.gpkg")
out_path <- paste0(path, "/level3_ts-pixels/X0016_Y0024_AVIRIS_ts.txt")
vi_extraction(root_path, vector_path, out_path)

root_path <- paste0(path, "/level3/X0017_Y0024")
vector_path <- paste0(path, "/AVIRIS-NG/X0017_Y0024_aviris.gpkg")
out_path <- paste0(path, "/level3_ts-pixels/X0017_Y0024_AVIRIS_ts.txt")
vi_extraction(root_path, vector_path, out_path)
