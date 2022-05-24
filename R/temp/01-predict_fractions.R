################################################################################
#' @title Predict fractions on map
################################################################################

#-------------------------------------------------------------------------------
# Library
library(terra)
library(data.table)
library(caret)
library(parallel)

#-------------------------------------------------------------------------------
# Source and model load
source("R/00-path_scenes.R")
healty <- readRDS("data/healty_model.rds")
wilted <- readRDS("data/wilted_model.rds")
dead <- readRDS("data/dead_model.rds")

#-------------------------------------------------------------------------------
# Arguments 
#' @param root_path Select the root path of where the scenes are located
#' @param out_path path of the outputs
#' @param overwrite if the file exist, do you want to create it again?
#' @param threads the number of threads to use for parallel processing

#-------------------------------------------------------------------------------
# Load
root_path <- "/media/antonio/antonio_ssd/level2"
out_path <- "/media/antonio/Work/force/level4"
overwrite <- TRUE
threads <- 16

#-------------------------------------------------------------------------------
# Function to apply
predict_fraction <- function(root_path, out_path, overwrite = FALSE, threads) {
  
  #Get scenes to work with
  frame <- path_scenes(root_path)
  
  files <- list.files("/media/antonio/antonio_ssd/level2", 
                      recursive = TRUE, 
                      full.names = TRUE, 
                      pattern = "BOA.tif") 
  
  #Mask function
  fmask <- function(x) {
    all(c(x) == 0)
  }
  
  #Function for model
  rfun <- function(mod, dat, ...) {
    ncls <- length(cls)
    nr <- nrow(dat)
    s <- split(dat, rep(1:ncls, each=ceiling(nr/ncls), length.out=nr))
    unlist(parallel::clusterApply(cls, s, function(x, ...) predict(mod, x, ...))  )
  }
  
  #Progress bar
  pb <- txtProgressBar(min = 0, max = length(files), style = 3)
  
  #nrow(frame)
  #Loop over scenes
  for(i in 1:length(files)) {
    
      #Read scene
      scene <- rast(paste0(files[i])) #This need to be corrected
      
      #Creat and apply mask
      logical_scene <- app(scene, fmask)
      scene_masked <- mask(scene, logical_scene, maskvalue = NA)
      
      rm("logical_scene")
      rm("scene")
      
      #Predict models on scenes
      cls <- makeCluster(threads, type = "FORK")
      clusterExport(cls, c("rfun", "healty", "wilted", "dead"))
      
      healty_scene <- predict(scene_masked, 
                              healty, 
                              fun = rfun)
      
      wilted_scene <- predict(scene_masked, 
                              wilted, 
                              fun = rfun)
      
      dead_scene <- predict(scene_masked, 
                              dead, 
                              fun = rfun)
      
      parallel::stopCluster(cls)
      
      scene_predicted <- c(healty_scene,
                           wilted_scene,
                           dead_scene)
      
    
  }
}

#-------------------------------------------------------------------------------
#' @example 
predict_fraction(root_path, out_path, overwrite = TRUE, threads = 16)