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

#' @param root_path: select the root path of where the .txt is located
#' @param out_path: path and name of the .txt outputs

path <- "/media/antonio/antonio_ssd/FORCE"

root_path <- paste0(path, "/model/data/X0015_0024_dkVI.txt")
out_path <- paste0(path, "/model/data/X0015_0024_dkVI_clean.txt")

data_clean(root_path, out_path)

#-------------------------------------------------------------------------------
#' Function

data_clean <- function(root_path, out_path) {
  
  #Read and add row number
  data <- fread(root_path)
  data$row <- 1:nrow(data)
  
  #Get pixel with the high weight
  unique_id <- unique(data$ID)
  frame <- data[, row[which.max(weight)], by = c("ID", "date")]
  colnames(frame)[3] <- "row"
  frame <- na.exclude(frame)
  
  #Select rows
  data <- data[frame$row, ]
  
  #Export
  fwrite(data, out_path, sep = "\t")

}
