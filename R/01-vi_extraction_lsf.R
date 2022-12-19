################################################################################
##### 03 - Extraction for pixels from L3 products
################################################################################

#' @description Batch extraction of values of VI using a vector file 
#' for training.

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
vector_path <- paste0(path, "/Training/2019/Points_training/X0014_Y0024.gpkg")
out_path <- paste0(path, "/level3_pixel-extraction/X0014_Y0024.txt")
year = c(2019, 2018) 
tile = "X0014_Y0024"
vi_extraction(root_path, vector_path, out_path, year, tile)

#-------------------------------------------------------------------------------
#Arguments

vi_extraction <- function(root_path, vector_path, out_path, year, tile) {
  
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
  frame <- subset(frame, VI == "KNV" | VI == "CCI")
  
  #Get method
  frame[, metric := substr(strsplit(scene, "_")[[1]][7], 1, 3), by = seq_along(1:nrow(frame))]
  
  #Order 
  frame <- frame[order(VI, metric)]
  
  #Get normalized mask for normalization
  kNDVI_frame <- subset(frame, VI == "KNV" & metric == "VPS")
  
  #Current
  mask_VPS <- rast(paste0(root_path, "/", kNDVI_frame$scene[1]))
  mask_VPS <- subset(mask_VPS, paste0("YEAR-", year[1]))
  mask_VPS[mask_VPS < 4000] <- 0
  mask_VPS[mask_VPS >= 4000] <- 1
  
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
                      xy = TRUE)
    colnames(values) <- c("ID", "target_year_value", "previus_year_value", "x", "y")

    values$VI <- frame$VI[i]
    values$metric <- frame$metric[i]
    values$target_year <- year[1]
    
    #Get normalization
    to_normalize <- mask(VI, mask_VPS, maskvalues = 0)
    
    #Target
    target_infinite <- is.infinite(to_normalize[][,1])
    target_year_mean <- mean(to_normalize[][,1][!target_infinite], na.rm = TRUE)
    target_year_sd <- sd(to_normalize[][,1][!target_infinite], na.rm = TRUE)
    
    #Previous
    previous_infinite <- is.infinite(to_normalize[][,2])
    previous_year_mean <- mean(to_normalize[][,2][!previous_infinite], na.rm = TRUE)
    previous_year_sd <- sd(to_normalize[][,2][!previous_infinite], na.rm = TRUE)
    
    #Add information to frame
    values$target_year_mean <- target_year_mean
    values$target_year_sd <- target_year_sd
    values$previous_year_mean <- previous_year_mean
    values$previous_year_sd <- previous_year_sd
    
    #Return
    extraction <- rbind(extraction, values)
    
  }
  
  # Change col names
  arrenge_col <- c("ID", "x", "y", "target_year", "VI", "metric", 
                   "target_year_value", "target_year_mean", "target_year_sd",
                   "previus_year_value", "previous_year_mean", "previous_year_sd")
  extraction <- extraction[, ..arrenge_col]
  
  #Prepare vector to merge
  vector <- as.data.frame(vector)
  vector$ID <- 1:nrow(vector)
  vector <- as.data.table(vector)
  vector$tile <- tile
  
  #Merge
  complete <- merge(vector, extraction, by = "ID", all.x = TRUE, all.y = TRUE)
  
  #Add names
  complete[condition == "1", Condition := "Healthy"]
  complete[condition == "2", Condition := "Symptomatic"]
  complete[condition == "3", Condition := "Dead"]
  
  #Order complete
  arrenge_col2 <- c("tile", "ID", "Condition", "x", "y", "target_year", "VI", "metric", 
                    "target_year_value", "target_year_mean", "target_year_sd",
                    "previus_year_value", "previous_year_mean", "previous_year_sd")
  complete <- complete[, ..arrenge_col2]
  
  #Export
  fwrite(complete, out_path, sep = "\t")
  
}

#' @example 
#' 
#' AVIRIS-NG 2018
root_path <- paste0(path, "/level3_lsf/X0014_Y0024")
vector_path <- paste0(path, "/OBSERVATIONS/2018_AVIRIS-NG/X0014_Y0024_aviris.gpkg")
out_path <- paste0(path, "/level3_lsf-pixels/AVIRIS-NG_2018/X0014_Y0024_2018_lsf.txt")
year = 2018 
tile = "X0014_Y0024"
vi_extraction(root_path, vector_path, out_path, year, tile)

root_path <- paste0(path, "/level3_lsf/X0015_Y0024")
vector_path <- paste0(path, "/OBSERVATIONS/2018_AVIRIS-NG/X0015_Y0024_aviris.gpkg")
out_path <- paste0(path, "/level3_lsf-pixels/AVIRIS-NG_2018/X0015_Y0024_2018_lsf.txt")
year = 2018 
tile = "X0015_Y0024"
vi_extraction(root_path, vector_path, out_path, year, tile)

root_path <- paste0(path, "/level3_lsf/X0016_Y0024")
vector_path <- paste0(path, "/OBSERVATIONS/2018_AVIRIS-NG/X0016_Y0024_aviris.gpkg")
out_path <- paste0(path, "/level3_lsf-pixels/AVIRIS-NG_2018/X0016_Y0024_2018_lsf.txt")
year = 2018 
tile = "X0016_Y0024"
vi_extraction(root_path, vector_path, out_path, year, tile)

root_path <- paste0(path, "/level3_lsf/X0017_Y0024")
vector_path <- paste0(path, "/OBSERVATIONS/2018_AVIRIS-NG/X0017_Y0024_aviris.gpkg")
out_path <- paste0(path, "/level3_lsf-pixels/AVIRIS-NG_2018/X0017_Y0024_2018_lsf.txt")
year = 2018 
tile = "X0017_Y0024"
vi_extraction(root_path, vector_path, out_path, year, tile)

#' AVIRIS-NG 2021
root_path <- paste0(path, "/level3_lsf/X0016_Y0024")
vector_path <- paste0(path, "/OBSERVATIONS/2021_AVIRIS-NG/X0016_Y0024_aviris.gpkg")
out_path <- paste0(path, "/level3_lsf-pixels/AVIRIS-NG_2021/X0016_Y0024_2021_lsf.txt")
year = 2021 
tile = "X0016_Y0024"
vi_extraction(root_path, vector_path, out_path, year, tile)

#' NAIP 2019
root_path <- paste0(path, "/level3_application/X0014_Y0024")
vector_path <- paste0(path, "/Training/2019/Points_training/X0014_Y0024.gpkg")
out_path <- paste0(path, "/level3_pixel-extraction/X0014_Y0024.txt")
year = c(2019, 2018) 
tile = "X0014_Y0024"
vi_extraction(root_path, vector_path, out_path, year, tile)

root_path <- paste0(path, "/level3_application/X0015_Y0024")
vector_path <- paste0(path, "/Training/2019/Points_training/X0015_Y0024.gpkg")
out_path <- paste0(path, "/level3_pixel-extraction/X0015_Y0024.txt")
year = c(2019, 2018) 
tile = "X0015_Y0024"
vi_extraction(root_path, vector_path, out_path, year, tile)

root_path <- paste0(path, "/level3_application/X0016_Y0024")
vector_path <- paste0(path, "/Training/2019/Points_training/X0016_Y0024.gpkg")
out_path <- paste0(path, "/level3_pixel-extraction/X0016_Y0024.txt")
year = c(2019, 2018) 
tile = "X0016_Y0024"
vi_extraction(root_path, vector_path, out_path, year, tile)

root_path <- paste0(path, "/level3_application/X0016_Y0025")
vector_path <- paste0(path, "/Training/2019/Points_training/X0016_Y0025.gpkg")
out_path <- paste0(path, "/level3_pixel-extraction/X0016_Y0025.txt")
year = c(2019, 2018) 
tile = "X0016_Y0025"
vi_extraction(root_path, vector_path, out_path, year, tile)

root_path <- paste0(path, "/level3_application/X0016_Y0027")
vector_path <- paste0(path, "/Training/2019/Points_training/X0016_Y0027.gpkg")
out_path <- paste0(path, "/level3_pixel-extraction/X0016_Y0027.txt")
year = c(2019, 2018) 
tile = "X0016_Y0027"
vi_extraction(root_path, vector_path, out_path, year, tile)

root_path <- paste0(path, "/level3_application/X0017_Y0024")
vector_path <- paste0(path, "/Training/2019/Points_training/X0017_Y0024.gpkg")
out_path <- paste0(path, "/level3_pixel-extraction/X0017_Y0024.txt")
year = c(2019, 2018) 
tile = "X0017_Y0024"
vi_extraction(root_path, vector_path, out_path, year, tile)

root_path <- paste0(path, "/level3_application/X0017_Y0026")
vector_path <- paste0(path, "/Training/2019/Points_training/X0017_Y0026.gpkg")
out_path <- paste0(path, "/level3_pixel-extraction/X0017_Y0026.txt")
year = c(2019, 2018) 
tile = "X0017_Y0026"
vi_extraction(root_path, vector_path, out_path, year, tile)

root_path <- paste0(path, "/level3_application/X0017_Y0027")
vector_path <- paste0(path, "/Training/2019/Points_training/X0017_Y0027.gpkg")
out_path <- paste0(path, "/level3_pixel-extraction/X0017_Y0027.txt")
year = c(2019, 2018) 
tile = "X0017_Y0027"
vi_extraction(root_path, vector_path, out_path, year, tile)
