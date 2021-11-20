################################################################################
####### List files
################################################################################

#Library
library(data.table)

#Select path
path <- "/media/antonio/Files/MN-Sentinel-Landsat/T15TVL"
path_elements <- length(strsplit(path, split = "/")[[1]])+1

#QUEUED SENTINEL-2--------------------------------------------------------------
files <- list.files(path = path, 
                    pattern = ".SAFE", 
                    all.files = TRUE,
                    full.names = TRUE, 
                    recursive = TRUE,
                    include.dirs = TRUE)

frame <- data.table(files = files, QUEUED = "QUEUED")

frame[ , date := as.Date(paste0(substr(strsplit(files, split = "/")[[1]][path_elements], 12, 15), "-",
                                   substr(strsplit(files, split = "/")[[1]][path_elements], 16, 17), "-",
                                   substr(strsplit(files, split = "/")[[1]][path_elements], 18, 19))),
       by = seq_len(nrow(frame))]

frame <- frame[order(date)]

#Export
fwrite(frame[, 1:2], "queue_T15TVL.txt", sep = " ")

#QUEUED Landsat-8---------------------------------------------------------------
path <- "/media/antonio/Files/MN-Sentinel-Landsat/Landsat"
path_elements <- length(strsplit(path, split = "/")[[1]])+1

files <- list.files(path = path, 
                    pattern = ".tar", 
                    all.files = TRUE,
                    full.names = TRUE, 
                    recursive = TRUE,
                    include.dirs = TRUE)

frame <- data.table(files = files, QUEUED = "QUEUED")

frame[ , date := as.Date(paste0(substr(strsplit(files, split = "/")[[1]][path_elements], 58, 61), "-",
                                substr(strsplit(files, split = "/")[[1]][path_elements], 62, 63), "-",
                                substr(strsplit(files, split = "/")[[1]][path_elements], 64, 65))),
       by = seq_len(nrow(frame))]

frame <- frame[order(date)]

#Export
fwrite(frame[, 1:2], "queue_landsat.txt", sep = " ")

#END