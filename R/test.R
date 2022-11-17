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

path <- "/media/antonio/antonio_ssd/TRAINING"
path <- "F:/TRAINING"

root_path <- paste0(path, "/level3_lsf/X0014_Y0024")
vector_path <- paste0(path, "/OBSERVATIONS/2018_AVIRIS-NG/X0014_Y0024_aviris.gpkg")
out_path <- paste0(path, "/level3_lsf-pixels/X0014_Y0024_AVIRIS_lsf.txt")
year = 2018 
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
  
  #Get method
  frame[, metric := substr(strsplit(scene, "_")[[1]][7], 1, 3), by = seq_along(1:nrow(frame))]
  
  #VPS kNDVI and CCI
  kNDVI_frame <- subset(frame, VI == "KNV" & metric == "VPS")
  
  #Get normalized
  mask_VPS <- rast(paste0(root_path, "/", kNDVI_frame$scene[1]))
  mask_VPS <- subset(mask_VPS, paste0("YEAR-", year))
  mask_VPS[mask_VPS < 4500] <- 0
  mask_VPS[mask_VPS >= 4500] <- 1
  
  #Order 
  frame <- subset(frame, VI == "CCI")
  frame <- frame[order(VI, metric)]
  
  #N scenes
  n_scenes <- nrow(frame)
  
  #Collect results
  extraction <- data.table()
  
  for(i in 1:n_scenes) {
    
    #Read
    VI <- rast(paste0(root_path, "/", frame$scene[i]))
    VI <- subset(VI, paste0("YEAR-", year))
    
    #Normalize value
    to_normalize <- mask(VI, mask_VPS, maskvalues = 0)
    vi_mean <- mean(to_normalize[], na.rm = TRUE)
    sd_mean <- sd(to_normalize[], na.rm = TRUE)
    
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
    values_melted$YOI <- year
    
    #Normalization
    values_melted$value <- (values_melted$value - vi_mean)/sd_mean
    
    #Remove residuals
    rm(list = c("VI", "values"))
    gc()
    
    #Return
    extraction <- rbind(extraction, values_melted)
    
  }
  
  #Change names
  colnames(extraction) <- c("ID", "x", "y", "year", "value", "VI", "metric")
  
  #Prepare vector to merge
  area <- expanse(vector)
  vector <- as.data.frame(vector)
  vector$ID <- 1:nrow(vector)
  vector <- as.data.table(vector)
  vector$tile <- tile
  #vector$quantile <- quantile
  #vector$area <- area
  
  #Merge
  complete <- merge(vector, extraction, by = "ID", all.x = TRUE, all.y = TRUE)
  
  #Add names
  complete[condition == "1", Condition := "Healthy"]
  complete[condition == "2", Condition := "Wilted"]
  complete[condition == "3", Condition := "Dead"]
  
  #Order complete
  #complete <- complete[, c(3, 1, 11, 5:6, 7, 4, 10, 8)]
  complete <- complete[, c("tile", "ID", "Condition", "x", "y", "year", "metric", "value")]
  
  #Modify year
  complete$year <- as.numeric(substr(complete$year, 6, 9))
  
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
root_path <- paste0(path, "/level3_lsf/X0014_Y0024")
vector_path <- paste0(path, "/OBSERVATIONS/2019_NAIP/X0014_Y0024/X0014_Y0024.gpkg")
out_path <- paste0(path, "/level3_lsf-pixels/NAIP_2019/X0014_Y0024_2019_lsf.txt")
year = 2019 
tile = "X0014_Y0024"
vi_extraction(root_path, vector_path, out_path, year, tile)

root_path <- paste0(path, "/level3_lsf/X0015_Y0024")
vector_path <- paste0(path, "/OBSERVATIONS/2019_NAIP/X0015_Y0024/X0015_Y0024.gpkg")
out_path <- paste0(path, "/level3_lsf-pixels/NAIP_2019/X0015_Y0024_2019_lsf.txt")
year = 2019 
tile = "X0015_Y0024"
vi_extraction(root_path, vector_path, out_path, year, tile)

root_path <- paste0(path, "/level3_lsf/X0016_Y0024")
vector_path <- paste0(path, "/OBSERVATIONS/2019_NAIP/X0016_Y0024/X0016_Y0024.gpkg")
out_path <- paste0(path, "/level3_lsf-pixels/NAIP_2019/X0016_Y0024_2019_lsf.txt")
year = 2019 
tile = "X0016_Y0024"
vi_extraction(root_path, vector_path, out_path, year, tile)

root_path <- paste0(path, "/level3_lsf/X0016_Y0025")
vector_path <- paste0(path, "/OBSERVATIONS/2019_NAIP/X0016_Y0025/X0016_Y0025.gpkg")
out_path <- paste0(path, "/level3_lsf-pixels/NAIP_2019/X0016_Y0025_2019_lsf.txt")
year = 2019 
tile = "X0016_Y0025"
vi_extraction(root_path, vector_path, out_path, year, tile)

root_path <- paste0(path, "/level3_lsf/X0016_Y0027")
vector_path <- paste0(path, "/OBSERVATIONS/2019_NAIP/X0016_Y0027/X0016_Y0027.gpkg")
out_path <- paste0(path, "/level3_lsf-pixels/NAIP_2019/X0016_Y0027_2019_lsf.txt")
year = 2019 
tile = "X0016_Y0027"
vi_extraction(root_path, vector_path, out_path, year, tile)

root_path <- paste0(path, "/level3_lsf/X0017_Y0024")
vector_path <- paste0(path, "/OBSERVATIONS/2019_NAIP/X0017_Y0024/X0017_Y0024.gpkg")
out_path <- paste0(path, "/level3_lsf-pixels/NAIP_2019/X0017_Y0024_2019_lsf.txt")
year = 2019 
tile = "X0017_Y0024"
vi_extraction(root_path, vector_path, out_path, year, tile)

root_path <- paste0(path, "/level3_lsf/X0017_Y0026")
vector_path <- paste0(path, "/OBSERVATIONS/2019_NAIP/X0017_Y0026/X0017_Y0026.gpkg")
out_path <- paste0(path, "/level3_lsf-pixels/NAIP_2019/X0017_Y0026_2019_lsf.txt")
year = 2019 
tile = "X0017_Y0026"
vi_extraction(root_path, vector_path, out_path, year, tile)

root_path <- paste0(path, "/level3_lsf/X0017_Y0027")
vector_path <- paste0(path, "/OBSERVATIONS/2019_NAIP/X0017_Y0027/X0017_Y0027.gpkg")
out_path <- paste0(path, "/level3_lsf-pixels/NAIP_2019/X0017_Y0027_2019_lsf.txt")
year = 2019 
tile = "X0017_Y0027"
vi_extraction(root_path, vector_path, out_path, year, tile)