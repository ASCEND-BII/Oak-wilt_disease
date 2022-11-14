################################################################################
##### 02 - Plot comparing training models
################################################################################

#' @description A script for ploting the comparison of training models.

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(caret)

#-------------------------------------------------------------------------------
# Root path

path <- "F:/TRAINING/level3_lsf-pixels"
path <- "/media/antonio/antonio_ssd/TRAINING/level3_lsf-pixels"

#-------------------------------------------------------------------------------
# Read models

models <- readRDS("data/models/models.rds")

#-------------------------------------------------------------------------------
# Ploting comparison 

#Resample
resamps <- resamples(models)
summary(resamps)

#Plot elements
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)

#Export figure

jpeg("model_comparisons.jpeg", quality = 100, res = 300, width = 150, height = 70, units = "mm", pointsize = 12) # JPEG device

  bwplot(resamps, layout = c(2, 1)) 

dev.off()
