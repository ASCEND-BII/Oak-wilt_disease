################################################################################
##### 03 - Plot of the classification assessment
################################################################################

#' @description A script for ploting the comparison of ROC curves.

#-------------------------------------------------------------------------------
#' Libraries

library(data.table)
library(ggplot2)
library(ggpubr)
library(multiROC)

#-------------------------------------------------------------------------------
# Get model of interest
rocs <- fread("data/models/rocs.csv")
cutoffs <- fread("data/models/cutoffs.csv")

#-------------------------------------------------------------------------------
# Interpolate sensitivity and specificity

roc_interpolation <- function(rocs) {
  
  rocs$observations <- paste0(rocs$type, "_", 
                              rocs$year, "_", 
                              rocs$tile, "_", 
                              rocs$Condition, "_",
                              rocs$repedition)
  
  times <- unique(rocs$observations)
  
  #Sequence to interpolate
  spec <- seq(-0.3, 1.3, by = 0.01)
  
  #Result collector
  results <- data.table()
  
  #Progress bar
  pb <- txtProgressBar(min = 0, max = length(times), style = 3)
  
  for(i in 1:length(times)) {
    
    # update progress bar
    setTxtProgressBar(pb, i)
    
    # Roc of interest
    sub_roc <- subset(rocs, observations == times[i])
    
    # Interpolation
    interpolate <- approx(x = 1 - sub_roc$Specificity, 
                          y = sub_roc$Sensitivity, 
                          xout = spec, 
                          method= "linear",
                          yleft = 0,
                          yright = 1)
    
    # Collect results
    sub_results <- data.table(type = rep(sub_roc$type[1], length(spec)),
                              year = rep(sub_roc$year[1], length(spec)),
                              tile = rep(sub_roc$tile[1], length(spec)),
                              repetition = rep(sub_roc$repedition[1], length(spec)),
                              Condition = rep(sub_roc$Condition[1], length(spec)),
                              AUC = rep(sub_roc$AUC[1], length(spec)),
                              Specificity = interpolate$x,
                              Sensitivity = interpolate$y)
    
    results <- rbind(results, sub_results)
    
  }
  
  return(results)
  
}

#-------------------------------------------------------------------------------
# Arrange the ROC for visualization

# Interpolate (time consuming)
interpolated_roc <- roc_interpolation(rocs)
fwrite(interpolated_roc, "data/models/rocs-interpolated.csv")

# Get summary
mean_roc <- interpolated_roc[, .(Sensitivity = mean(Sensitivity), AUC = mean(AUC)), 
                             by = c("type", "year", "tile", "Condition", "Specificity")]

# Create Factors
mean_roc$Condition <- as.factor(mean_roc$Condition)
mean_roc$Condition <- factor(mean_roc$Condition, 
                             levels = c("Healthy", "Symptomatic", "Dead", "Macro", "Micro"))

mean_roc$tile <- as.factor(mean_roc$tile)
mean_roc$tile <- factor(mean_roc$tile, levels = c("All",
                                                  "X0014_Y0024",
                                                  "X0015_Y0024",
                                                  "X0016_Y0024",
                                                  "X0017_Y0024",
                                                  "X0016_Y0025",
                                                  "X0017_Y0026",
                                                  "X0016_Y0027",
                                                  "X0017_Y0027"))

#-------------------------------------------------------------------------------
# Arrange the ROC cutoff for visualization

mean_cutoff <- cutoffs[, .(Sensitivity = mean(Sensitivity), 
                           Specificity = mean(Specificity), 
                           Cutoff = mean(Cutoff),
                           sd_sensitivity = sd(Sensitivity),
                           sd_specificity = sd(Specificity),
                           sd_cutoff = sd(Cutoff)), 
                           by = c("type", "year", "tile", "Condition")]

# Create Factors
mean_cutoff$Condition <- as.factor(mean_cutoff$Condition)
mean_cutoff$Condition <- factor(mean_cutoff$Condition, 
                             levels = c("Healthy", "Symptomatic", "Dead"))

mean_cutoff$tile <- as.factor(mean_cutoff$tile)
mean_cutoff$tile <- factor(mean_cutoff$tile, levels = c("All",
                                                  "X0014_Y0024",
                                                  "X0015_Y0024",
                                                  "X0016_Y0024",
                                                  "X0017_Y0024",
                                                  "X0016_Y0025",
                                                  "X0017_Y0026",
                                                  "X0016_Y0027",
                                                  "X0017_Y0027"))

#Layout properties -------------------------------------------------------------

tamano <- 14
tamano2 <- 12

th <- theme_bw(base_size = tamano) + theme(plot.background = element_blank(),
                                           panel.grid.major = element_blank(),
                                           panel.grid.minor = element_blank(),
                                           plot.margin = margin(5, 6, 5, 5, "pt"),
                                           axis.text.x = element_text(color = "black", size = tamano2),
                                           axis.text.y = element_text(color = "black", size = tamano2),
                                           strip.text.x = element_text(size = tamano, color = "black"),
                                           strip.text.y = element_text(size = tamano, color = "black"),
                                           strip.background = element_rect(color= "black", fill="grey90", linetype="solid"))

#Plot --------------------------------------------------------------------------
# Spatial variation

data_spatial <- mean_roc[tile != "All"]
data_spatial <- data_spatial[type == "Testing"]
data_spatial <- data_spatial[year == "2019"]

roc_spatial <- mean_cutoff[tile != "All"]
roc_spatial <- roc_spatial[type == "Testing"]
roc_spatial <- roc_spatial[year == "2019"]

plot <- ggplot(data_spatial, aes(x = Specificity, y=Sensitivity, colour = tile)) +
  geom_path(aes(color = tile), size = 0.4) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotted') +
  geom_errorbar(data = roc_spatial, aes(ymin= Sensitivity - sd_sensitivity, ymax = Sensitivity + sd_sensitivity)) + 
  geom_errorbarh(data = roc_spatial, aes(xmin= Specificity - sd_specificity, xmax = Specificity + sd_specificity)) + 
  geom_point(data = roc_spatial, aes(x = Specificity, y = Sensitivity, colour = tile), size = 2) +
  xlab("1 - Specificity") +
  th + 
  scale_x_continuous(limits = c(-0.02, 1.01), expand = c(0, 0.01)) +
  scale_y_continuous(limits = c(-0.02, 1.02), expand = c(0, 0.01)) +
  scale_colour_viridis_d("Tile", option = "mako", direction = -1) +
  facet_wrap(. ~ Condition) +
  theme(legend.position="none")
  

jpeg("figures/spatial_roc.jpeg", quality = 100, res = 600, width = 210, height = 150, units = "mm", pointsize = 12) # JPEG device

plot

dev.off()

#Plot --------------------------------------------------------------------------
# Temporal variation

data_temporal <- mean_roc[tile == "All"]
data_temporal <- data_temporal[type == "Testing"]
data_temporal$year <- as.factor(data_temporal$year)
data_temporal$year <- factor(data_temporal$year, levels = c("2018", "2019", "2021"))

roc_temporal <- mean_cutoff[tile == "All"]
roc_temporal <- roc_temporal[type == "Testing"]
roc_temporal$year <- as.factor(roc_temporal$year)
roc_temporal$year <- factor(roc_temporal$year, levels = c("2018", "2019", "2021"))

plot <- ggplot(data_temporal, aes(x = Specificity, y=Sensitivity, colour = year)) +
  geom_path(aes(color = year), size = 0.4) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotted') +
  geom_errorbar(data = roc_temporal, aes(ymin= Sensitivity - sd_sensitivity, ymax = Sensitivity + sd_sensitivity)) + 
  geom_errorbarh(data = roc_temporal, aes(xmin= Specificity - sd_specificity, xmax = Specificity + sd_specificity)) + 
  geom_point(data = roc_temporal, aes(x = Specificity, y= Sensitivity, colour = year), size = 2) +
  xlab("1 - Specificity") +
  th + 
  scale_x_continuous(limits = c(-0.02, 1.01), expand = c(0, 0.01)) +
  scale_y_continuous(limits = c(-0.02, 1.02), expand = c(0, 0.01)) +
  scale_colour_manual("Year", values = c("#4575b4", "grey40", "#d73027")) +
  facet_wrap(. ~ Condition) +
  theme(legend.position="none")

jpeg("figures/temporal_roc.jpeg", quality = 100, res = 600, width = 240, height = 125, units = "mm", pointsize = 12) # JPEG device

plot

dev.off()
