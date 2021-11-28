################################################################################
##### Estimation of trends using Sentinel-2 time series derived from FORCE
################################################################################

#Library
library(gdalcubes)
library(data.table)
library(raster)

#Selection of BOA per tiles
root_path <- "/media/antonio/Work/Oak-Sentinel/level3_sen2"
tile <- "X0034_Y0049"
files <- list.files(path = paste0(root_path, "/", tile), 
                    pattern = ".tif", 
                    all.files = TRUE,
                    full.names = FALSE, 
                    recursive = TRUE)

#Cleaning of BOA scenes
frame_files <- data.table(files = files)

frame_files[, sensor := strsplit(files, "_")[[1]][3], 
            by = seq_along(1:nrow(frame_files))]

frame_files <- frame_files[sensor != "LND08",] #Remove Landsat-8
frame_files <- frame_files[1:10,] #Needs to be deleted

#Creating data cubes
SEN2 <- create_image_collection(paste0(root_path, "/", tile, "/", frame_files$files), 
                                format = "misc/force_L3_SEN2.json", 
                                out_file = "L8.db")















library(raster)





