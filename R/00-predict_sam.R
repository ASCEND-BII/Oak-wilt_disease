################################################################################
##### A function to predict SAM values from models
################################################################################

#-------------------------------------------------------------------------------
# Libraries

library(terra)
library(caret)

#-------------------------------------------------------------------------------
# Arguments

#' @param healty_model healthy model
#' @param wilted_model Wilted model
#' @param dead_model Dead model

#-------------------------------------------------------------------------------
# Load models

healty_model <- readRDS("data/healty_model.rds")
wilted_model <- readRDS("data/wilted_model.rds")
dead_model <- readRDS("data/dead_model.rds")

#-------------------------------------------------------------------------------
#Functions
predfun <- function(data, healty_model, wilted_model, dead_model, threads = 28) { 
  
  #Predict models
  healty <- predict(data, healty_model, na.rm = TRUE)
  wilted <- predict(data, wilted_model, na.rm = TRUE)
  dead <- predict(data, dead_model, na.rm = TRUE)
  
  #Stack and name
  model <- c(healty, wilted, dead)
  names(model) <- c("healty", "wilted", "dead")
  
  return(model)
}
