################################################################################
##### Estimation of trends using Sentinel-2 time series derived from FORCE
################################################################################

#Library
library(gdalcubes)
library(data.table)

#Sentinel-2 imagery
root_path <- "/media/antonio/antonio_ssd/force"
tile <- "X0034_Y0049"
files <- list.files(path = paste0(root_path, "/", tile), 
                    pattern = "_BOA.tif", 
                    all.files = TRUE,
                    full.names = FALSE, 
                    recursive = TRUE)

frame_files <- data.table(files = files)

frame_files[, sensor := strsplit(files, "_")[[1]][3], 
            by = seq_along(1:nrow(frame_files))]

frame_files <- frame_files[sensor != "LND08",]
frame_files <- frame_files[1:10,]

#Creating data cubes
SEN2 <- create_image_collection(paste0(root_path, "/", tile, "/", frame_files$files), 
                                format = "force_L2_SEN2", 
                                out_file = "L8.db")
