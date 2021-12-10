################################################################################
##### Extraction of values from point of interest
################################################################################

#-------------------------------------------------------------------------------
# Source load

source("R/00-path_scenes.R")

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

root_path <- "/media/antonio/Work/Oak-wilt/level3_sen3"
points <- fread("/media/antonio/Work/Oak-wilt/misc/oak_trees/test.csv")
tile_sel <- "X0009_Y0014"
layer <- "KNV"

#-------------------------------------------------------------------------------
#Functions
poi_extraction <- function(root_path, points, tile_sel, layer) {
  
  frame <- path_scenes(root_path)
  frame <- subset(frame, band == layer)
  frame <- subset(frame, tile == tile_sel)
  
  scenes <- paste0(root_path, "/", frame$tile, "/", frame$scene)
  
  #Create a raster stack
  scenes_rast <- rast(scenes)
  names(scenes_rast) <- frame$date
  
  #Create points
  xy <- vect(points, geom = c("x", "y"), crs = crs(scenes_rast))
  
  #Extract values
  values <- terra::extract(scenes_rast, xy, xy = T, method = "simple")
  values <- as.data.table(cbind(values, points[,3]))
  frame <- melt(values, id.vars = c("ID", "x", "y", "Status"),
                variable.name = "date", value.name = "layer")
  frame$date <- as.Date(as.character(frame$date))
  frame <- na.exclude(frame)
  frame$layer <- frame$layer/10000
  
  frame <- frame[order(ID, date)]
  
  return(frame)
}

