################################################################################
#' @title Plot of training model
################################################################################

#-------------------------------------------------------------------------------
# Library
library(data.table)
library(ggplot2)
library(ggpubr)
library(ggExtra)

#-------------------------------------------------------------------------------
# Arguments 
#' @param data A data.table with the results of spectral mixing
#' @param model Trained model from GPR
#' @param fill Color code for the figure

#-------------------------------------------------------------------------------
# Plot function

predicted_plot <- function(data, model, fill = "#1b9e77") {
  
  fa <- data
  #fa[, NDBI := (SWIR1 - NIR) / (SWIR1 + NIR)]
  #fa[, CIre := (REDEDGE3 / REDEDGE1) - 1]
  #fa <- fa[, c(1, 6, 7)]
  fa$predicted <- predict(model)
  
  fa <- fa[predicted <= 1]
  
  #Model performance
  performance <- round(model$results[2:6], 2)
  
  df <- data.frame(
      x = c(0.2, 0.2, 0.2),
      y = c(0.8, 0.75, 0.7),
      text = c(paste("R2","=", performance$Rsquared),
               paste("RMSE" ,"=" , performance$RMSE),
               paste("MAE" ,"=" , performance$MAE))
  )
  
  th <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  #plot
  plot <- ggplot(fa, aes(x = predicted, y = SAM)) +
    geom_point(shape = 21, colour = "white", fill = fill, size = 1.5, alpha = 0.75) +
    theme_bw() + th +
    ylab("Observed fraction") +
    xlab("Predicted fraction") +
    scale_x_continuous(limits = c(0.0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.0, 1), expand = c(0, 0)) +
    geom_abline(intercept = 0, slope = 1, colour = "grey", linetype = "dotted") +
    geom_smooth(method = "lm", se = FALSE, colour = "black", fullrange = TRUE) +
    geom_text(data = df, aes(x, y, label = text), size = 3)
  
  return(plot)
  
}

#Load data
#Healthy
healty_ssam <- fread("data/healty_ssam.csv")
healty_model <- readRDS("data/healty_model.rds")
healty_plot <- predicted_plot(healty_ssam, healty_model, fill = "#1b9e77")

#OW
wilted_ssam <- fread("data/wilted_ssam.csv")
wilted_model <- readRDS("data/wilted_model.rds")
wilted_plot <- predicted_plot(wilted_ssam, wilted_model, fill = "#e7298a")

#Dead
dead_ssam <- fread("data/dead_ssam.csv")
dead_model <- readRDS("data/dead_model.rds")
dead_plot <- predicted_plot(dead_ssam, dead_model, fill = "#7570b3")


###Arrange plot-----------------------------------------------------------------
yo <- ggarrange(healty_plot,
                wilted_plot,
                dead_plot,
                labels = c("Healthy", "Wilted", "Dead"), 
                font.label=list(color="black", size= 12, face = "plain"),
                ncol = 3,
                label.x = 0.2,
                label.y = 0.95)

###Export-----------------------------------------------------------------------
jpeg("Figure_performance.jpg", width = 21, height = 7, units = "cm", res = 600, pointsize = 7)

yo

dev.off()
