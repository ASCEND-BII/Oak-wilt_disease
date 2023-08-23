################################################################################
##### 03 - Extraction for pixels from L3 products
################################################################################

#' @description Batch extraction of pixels from LSP scenes using a vector file.

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(terra)

#-------------------------------------------------------------------------------
#Arguments

#' @param root_path: select the root path of where the scenes are located
#' @param vector_path: path of the ..gpkg file
#' @param out_path: path and name of the .txt outputs
#' @param threads: the number of threads to use for parallel processing

path <- "/media/antonio/Work/Projects/Oak-wilt_mapping"
root_path <- paste0(path, "/level3_application/X0014_Y0024")
vector_path <- paste0(path, "/Training/2019/Polygon_training/X0014_Y0024_polygon.gpkg")
out_path <- paste0(path, "/level3_pixel-extraction/X0014_Y0024_polygon.txt")
year = c(2019) 
tile = "X0014_Y0024"
vi_extraction_polygon(root_path, vector_path, out_path, year, tile)

#-------------------------------------------------------------------------------
#Arguments

vi_extraction_polygon <- function(root_path, vector_path, out_path, year, tile) {
  
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
  frame <- frame[VI == "CCI"]
  
  #Get normalized mask for normalization
  VI_mask <- subset(frame, VI == "CCI" & metric == "VSS")
  
  #Current
  VI_mask <- rast(paste0(root_path, "/", VI_mask$scene[1]))
  VI_mask <- subset(VI_mask, paste0("YEAR-", year[1]))
  VI_mask[VI_mask < 2500] <- 0
  VI_mask[VI_mask >= 2500] <- 1
  
  #Order 
  frame <- frame[order(VI, metric)]
  
  #N scenes
  n_scenes <- nrow(frame)
  
  #Collect results
  extraction <- data.table()
  
  for(i in 1:n_scenes) {
    
    #Read
    VI <- rast(paste0(root_path, "/", frame$scene[i]))
    years <- c(paste0("YEAR-", year)) #, paste0("YEAR-", (year-1))
    VI <- subset(VI, years)
    
    #Extract values
    values <- extract(VI, 
                      vector, 
                      method = "simple",
                      xy = TRUE,
                      exact = TRUE)
    colnames(values) <- c("ID", "value", "x", "y", "fraction")
    
    values$VI <- frame$VI[i]
    values$metric <- frame$metric[i]
    values$year <- year[1]
    
    #Get normalization
    to_normalize <- mask(VI, VI_mask, maskvalues = 0)
    
    #Target
    target_infinite <- is.infinite(to_normalize[][,1])
    year_mean <- mean(to_normalize[][,1][!target_infinite], na.rm = TRUE)
    year_sd <- sd(to_normalize[][,1][!target_infinite], na.rm = TRUE)
    year_density <- density(to_normalize[][,1][!target_infinite], 
                            na.rm = TRUE,
                            n = 1000)
    year_density <- year_density$x[which.max(year_density$y)]
    
    #Add information to frame
    values$scene_mean <- year_mean
    values$scene_sd <- year_sd
    values$scene_peak <- year_density
    
    #Return
    extraction <- rbind(extraction, values)
    
  }
  
  # Change col names
  arrenge_col <- c("ID", "x", "y", "fraction", "year", "VI", "metric", 
                   "value", "scene_mean", "scene_sd", "scene_peak")
  extraction <- extraction[, ..arrenge_col]
  
  #Prepare vector to merge
  vector <- as.data.frame(vector)
  vector$ID <- 1:nrow(vector)
  vector <- as.data.table(vector)
  vector$tile <- tile
  
  #Merge
  complete <- merge(vector, extraction, by = "ID", all.x = TRUE, all.y = TRUE)
  
  #Add names
  #complete[condition == "1", Condition := "Healthy"]
  complete[condition == "2", Condition := "Symptomatic"]
  #complete[condition == "3", Condition := "Dead"]
  
  #Order complete
  arrenge_col2 <- c("tile", "ID", "Condition", "x", "y", "area", "fraction", "VI", "metric", 
                    "value", "scene_mean", "scene_sd", "scene_peak")
  complete <- complete[, ..arrenge_col2]
  
  #Export
  fwrite(complete, out_path, sep = "\t")
  
}

#' @example 
#' 
#' NAIP 2019
path <- "/media/antonio/Work/Projects/Oak-wilt_mapping"
root_path <- paste0(path, "/level3_application/X0014_Y0024")
vector_path <- paste0(path, "/Training/2019/Polygon_training/X0014_Y0024_polygon.gpkg")
out_path <- paste0(path, "/level3_pixel-extraction/X0014_Y0024_polygon.txt")
year = c(2019) 
tile = "X0014_Y0024"
vi_extraction_polygon(root_path, vector_path, out_path, year, tile)

root_path <- paste0(path, "/level3_application/X0015_Y0024")
vector_path <- paste0(path, "/Training/2019/Polygon_training/X0015_Y0024_polygon.gpkg")
out_path <- paste0(path, "/level3_pixel-extraction/X0015_Y0024_polygon.txt")
year = c(2019)
tile = "X0015_Y0024"
vi_extraction_polygon(root_path, vector_path, out_path, year, tile)

root_path <- paste0(path, "/level3_application/X0016_Y0024")
vector_path <- paste0(path, "/Training/2019/Polygon_training/X0016_Y0024_polygon.gpkg")
out_path <- paste0(path, "/level3_pixel-extraction/X0016_Y0024_polygon.txt")
year = c(2019)
tile = "X0016_Y0024"
vi_extraction_polygon(root_path, vector_path, out_path, year, tile)
