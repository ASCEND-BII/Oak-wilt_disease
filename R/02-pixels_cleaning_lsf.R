################################################################################
##### 02 - Data cleaning for developing a model
################################################################################

#' @description A script for cleaning the pixels observations for future model 
#' development.

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(ggridges)
library(ggplot2)

#-------------------------------------------------------------------------------
#Paths
path <- "/media/antonio/Work/Projects/Oak-wilt_mapping/level3_pixel-extraction"

#-------------------------------------------------------------------------------
# Compile observations

files <- list.files(path = path, 
                    pattern = ".txt", 
                    all.files = TRUE,
                    full.names = FALSE, 
                    recursive = TRUE)

#Arrange path in frame
frame <- data.table(matrix(unlist(strsplit(files, "/")), 
                           nrow= length(files), 
                           byrow=TRUE), stringsAsFactors=FALSE)
colnames(frame) <- c("folder", "file")

#Read in a loop
data <- data.table()

for(i in 1:nrow(frame)) {
  
  file <- fread(paste0(path, "/", frame$folder[i], "/", frame$file[i]))
  file$dataset <- frame$folder[i]
  data <- rbind(data, file)

}

#Normalize values
data$normalized <- (data$value-data$scene_mean)/data$scene_sd

#Create unique IDs
ID <- unique(data[,c("tile", "Condition", "dataset", "x", "y", "ID")])
ID$sample <- 1:nrow(ID)

#Merge
data <- merge(ID, data, by = c("tile", "Condition", "dataset", "x", "y", "ID"), all.x = TRUE, all.y = TRUE)
data$ID <- data$sample

data <- data[, c("tile", "ID", "Condition", "dataset", "x", "y", "VI", "metric", 
                 "value", "scene_mean", "scene_sd", "scene_peak", "normalized")]

#Export
data <- na.exclude(data)
data <- data[order(tile, ID, Condition, dataset, x, y)]
fwrite(data, paste0(path, "/master_file.csv"))

#-------------------------------------------------------------------------------
#Master training
master <- data

#Reshape frame
kepp <- c("tile", "ID", "Condition", "dataset", "x", "y", "VI",  "metric", "value") #target_normalized
master <- master[, ..kepp]

master_dcast <- dcast(master, tile + ID + Condition + x + y + dataset + VI ~ metric)
master_dcast <- master_dcast[VI == "CCI"]
kepp <- c("tile", "ID", "Condition", "x", "y", "dataset", "VI", unique(master$metric))
master_dcast <- master_dcast[, ..kepp]
master_dcast <- na.exclude(master_dcast)
master_dcast$Condition <- as.factor(master_dcast$Condition)
master_dcast$Condition <- factor(master_dcast$Condition, 
                                 levels = c("Healthy", "Symptomatic", "Dead"))

#Export
fwrite(master_dcast, paste0(path, "/master_training.csv")) #Manual check the values
master_dcast <- fread(paste0(path, "/master_training.csv"))
#-------------------------------------------------------------------------------
#Normalized training 
normalized <- data

#Reshape frame
kepp <- c("tile", "ID", "Condition", "x", "y", "dataset", "VI",  "metric", "normalized")
normalized <- normalized[, ..kepp]

normalized_dcast <- dcast(normalized, tile + ID + Condition + x + y + dataset + VI ~ metric)
normalized_dcast <- normalized_dcast[VI == "CCI"]
kepp <- c("tile", "ID", "Condition", "x", "y", "dataset", "VI", unique(master$metric))
normalized_dcast <- normalized_dcast[, ..kepp]

#Merge with non-normalized
normalized_dcast <- merge(master_dcast[, c("tile", "ID")], normalized_dcast, 
                          by = c("tile", "ID"),
                          all.x = TRUE, all.y = FALSE)

normalized_dcast$Condition <- as.factor(normalized_dcast$Condition)
normalized_dcast$Condition <- factor(normalized_dcast$Condition, 
                                 levels = c("Healthy", "Symptomatic", "Dead"))

fwrite(normalized_dcast, paste0(path, "/master_normalized.csv"))

