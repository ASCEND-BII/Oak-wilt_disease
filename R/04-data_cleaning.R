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

path <- "/media/antonio/antonio_ssd/FORCE"

root_path <- paste0(path, "/level3_shifted/X0014_0024_VI.txt")
out_path <- paste0(path, "/level3_shifted/X0014_0024_VI_clean.txt")
select_method <- "TSI"

data_clean(root_path, out_path)

#-------------------------------------------------------------------------------
#' Function

data_clean <- function(root_path, out_path) {
  
  #Read and add row number
  data <- fread(root_path)
  data <- subset(data, method == select_method)
  
  #dcast
  data <- dcast(data, ID + condition + date + area + x + y + weight ~ VI, value.var = "value")
  data$row <- 1:nrow(data)
  
  #Get pixel with the high weight
  frame <- data[, row[which.max(weight)], by = c("ID", "date")]
  colnames(frame)[3] <- "row"
  frame <- na.exclude(frame)
  
  #Select rows
  data <- data[frame$row, .SD, .SDcols = c(1:11)]
  
  #Export
  fwrite(data, out_path, sep = "\t")
  
}
