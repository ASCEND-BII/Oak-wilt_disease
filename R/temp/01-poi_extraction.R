################################################################################
##### Extraction of values from point of interest
################################################################################

#-------------------------------------------------------------------------------
# Source load

source("R/00-path_vi_scenes.R")

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(terra)

#-------------------------------------------------------------------------------
#Arguments

#' @param root_path select the root path of where the scenes are located
#' @param points a data.table describing the location of the points of interest
#' @param tile_sel select the tile of interest
#' @param layer select the layer/band of interest

root_path <- "/media/antonio/antonio_ssd/level3"
points <- fread("/home/antonio/Documents/GIS/oak_presentation.csv")
tile_sel <- "X0016_Y0024"
layer <- "N1N"

#-------------------------------------------------------------------------------
#Functions
poi_extraction <- function(root_path, points, tile_sel, layer) {
  
  frame <- path_vi_scenes(root_path)
  frame <- subset(frame, VI == layer)
  frame <- subset(frame, tile == tile_sel)
  
  scenes <- paste0(root_path, "/", frame$tile, "/", frame$scene)
  
  #Create a raster stack
  scenes_rast <- rast(scenes)
  names(scenes_rast) <- frame$date
  
  #Create points
  xy <- vect(points, geom = c("x", "y"), crs = crs(scenes_rast))
  
  #Extract values
  values <- terra::extract(scenes_rast, xy, xy = T, method = "simple")
  values <- as.data.table(cbind(values, points[,2]))
  frame_melt <- melt(values, id.vars = c("ID", "x", "y", "condition"),
                variable.name = "date", value.name = "value")
  frame_melt$date <- as.Date(as.character(frame_melt$date))
  frame_melt <- na.exclude(frame_melt)
  frame_melt$value <- frame_melt$value/10000
  
  frame_melt <- frame_melt[order(ID, date, condition)]
  
  return(frame)
  
}

