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
frame_files <- frame_files[95:100,]

#Creating data cubes
SEN2 <- create_image_collection(paste0(root_path, "/", tile, "/", frame_files$files), 
                                format = "/home/antonio/Documents/Github/Oak-wilt/misc/force_L2_SEN2.json", 
                                out_file = "L8.db")

SEN2
plot(rgb=3:1, SEN2)


raster_cube(SEN2) |>
  select_bands(c("SWIR1")) |>
  plot(nbreaks=10, key.pos=1)
