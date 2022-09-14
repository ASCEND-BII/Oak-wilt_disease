################################################################################
##### 04 - Data manage for developing a model
################################################################################

#' @description Batch extraction of values of VI using a vector file 
#' for training.

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(terra)

#-------------------------------------------------------------------------------
#Arguments

#' @param root_path select the root path of where the .txt is located
#' @param out_path path and name of the .txt outputs
#' @param select_method Method to subset

path <- "/media/antonio/antonio_ssd/FORCE/corregistration/application/level3_shifted_phenology"

root_path <- paste0(path, "/X0014_0024_phenology.txt")
out_path <- paste0(path, "/X0014_0024_phenology_clean.txt")
tile <- "X0014_0024"

data_clean(root_path, out_path, tile)

#-------------------------------------------------------------------------------
#' Function

data_clean <- function(root_path, out_path, tile) {
  
  #Read and add row number
  data <- fread(root_path)
  
  #Change year
  data$year <- as.numeric(substr(data$year, 6, 9))
  
  #dcast
  data <- dcast(data, condition + ID + area + x + y + weight + year + VI ~ metric)
  
  #Clean -10000
  #data <- data[DSS >= 0]
  
  #add tile
  data$tile <- tile
  
  #data$row <- 1:nrow(data)
  
  #Get pixel with the high weight
  #frame <- data[, row[which.max(weight)], by = c("ID", "date")]
  #colnames(frame)[3] <- "row"
  #frame <- na.exclude(frame)
  
  #Select rows
  #data <- data[frame$row, .SD, .SDcols = c(1:11)]
  
  #Export
  fwrite(data, out_path, sep = "\t")
  
}
