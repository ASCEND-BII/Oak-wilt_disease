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

root_path <- "F:/FORCE/level3_shifted/X0015_0024_VI.txt"
out_path <- "F:/FORCE/level3_shifted/X0015_0024_VI_clean.txt"

#-------------------------------------------------------------------------------
#' Function

data_clean <- function(root_path, out_path) {
  
  #Read and add row number
  data <- fread(root_path)
  data$row <- 1:nrow(data)
  
  #Get pixel with the high weight
  unique_id <- unique(data$ID)
  frame <- data[, row[which.min(CCI)], by = c("ID", "date")]
  colnames(frame)[3] <- "row"
  frame <- na.exclude(frame)
  
  #Select rows
  data <- data[frame$row, ]
  
  #Export
  fwrite(data, out_path, sep = "\t")

}