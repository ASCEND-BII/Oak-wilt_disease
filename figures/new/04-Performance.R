################################################################################
##### 04 - Plot of performance metrics
################################################################################

#' @description A script for ploting the performance of the metrics.

#-------------------------------------------------------------------------------
#' Libraries

library(data.table)
library(ggplot2)
library(ggpubr)
library(Rmisc)

#-------------------------------------------------------------------------------
# Root path

path <- "F:/TRAINING/level3_lsf-pixels"
path <- "/media/antonio/antonio_ssd/TRAINING/level3_lsf-pixels"

#-------------------------------------------------------------------------------
# Layout properties

pa <- c("#1b9e77", "#d95f02", "#7570b3")
tamano <- 14
tamano2 <- 12

th <- theme_bw(base_size = tamano) + theme(plot.background = element_blank(),
                                           panel.grid.major = element_blank(),
                                           panel.grid.minor = element_blank(),
                                           plot.margin = margin(4, 6, 0, 0, "pt"),
                                           axis.text.x = element_text(color = "black", size = tamano2),
                                           axis.text.y = element_text(color = "black", size = tamano2),
                                           strip.text.x = element_text(size = tamano, color = "black"),
                                           strip.text.y = element_text(size = tamano, color = "black"),
                                           strip.background = element_rect(color= "black", fill="grey90", linetype="solid"))

#-------------------------------------------------------------------------------
# Plot

data <- fread("data/models/byClass.csv")

data[Class == "Wilted", Class := "Symptomatic"]
data$Class <- as.factor(data$Class)
data$Class <- factor(data$Class, levels = c("Healthy", "Symptomatic", "Dead"))

data$Year <- as.factor(data$Year)
data$Year <- factor(data$Year, levels = c("2018", "2019", "2021"))

data$Tile <- as.factor(data$Tile)
data$Tile <- factor(data$Tile, levels = c("All", "X0014_Y0024", "X0015_Y0024", 
                                          "X0016_Y0024", "X0016_Y0025", "X0016_Y0027",
                                          "X0017_Y0024", "X0017_Y0026"))

# Temporal variation
data_temporal <- data[Tile == "All"]
data_temporal <- data_temporal[Type == "Testing"]

data_temporal <- data_temporal[, c("Year", "Class", "Balanced Accuracy", 
                                   "Sensitivity", "Specificity", "F1")]
data_temporal <- melt(data_temporal, id.vars = c("Year", "Class"),
                      measure.vars = c("Balanced Accuracy", "Sensitivity", "Specificity", "F1"),
                      variable.name = "Metric",
                      value.name = "value")
colnames(data_temporal)[2] <- "Condition"
data_temporal <- data_temporal[order(Year, Condition, Metric)]

summary_gruops <- summarySE(data_temporal, measurevar = "value", groupvars=c("Metric"))

summary_condition <- summarySE(data_temporal, measurevar = "value", groupvars=c("Year",
                                                                                "Condition",
                                                                                "Metric"))


plot <- ggplot() +
  geom_hline(data = summary_gruops, aes(yintercept = value), linetype = "dotted", 
             color = "black", size = 0.4) +
  geom_errorbar(data = summary_condition, aes(ymin= value-sd, ymax= value+sd, 
                                              x = Year, colour = Condition, 
                                              group = Condition), width=.2,
                position=position_dodge(0.1)) +
  geom_point(data = summary_condition, aes(y = value, x = Year, colour = Condition, 
                                           group = Condition, shape = Condition), 
             size = 3, position=position_dodge(0.1)) +
  geom_path(data = summary_condition, aes(y = value, x = Year, group = Condition, 
                                          colour = Condition), linetype = "dashed") +
  th + ylab("Value") + xlab("Observation Year") +
  scale_colour_manual(values = pa) +
  coord_cartesian(xlim = c(1, 3)) +
  facet_wrap(. ~ Metric)

jpeg("temporal_performance.jpeg", quality = 100, res = 600, width = 210, height = 150, units = "mm", pointsize = 12) # JPEG device

plot

dev.off()

# Spatial variation

data_spatial <- data[Tile != "All"]
data_spatial <- data_spatial[Type == "Testing"]

data_spatial <- data_spatial[, c("Tile", "Class", "Balanced Accuracy", 
                                   "Sensitivity", "Specificity", "F1")]
data_spatial <- melt(data_spatial, id.vars = c("Tile", "Class"),
                      measure.vars = c("Balanced Accuracy", "Sensitivity", "Specificity", "F1"),
                      variable.name = "Metric",
                      value.name = "value")
colnames(data_spatial )[2] <- "Condition"
data_spatial  <- data_spatial[order(Tile, Condition, Metric)]

summary_gruops <- summarySE(data_spatial , measurevar = "value", groupvars=c("Metric"))

summary_condition <- summarySE(data_spatial , measurevar = "value", groupvars=c("Tile",
                                                                                "Condition",
                                                                                "Metric"))


plot <- ggplot() +
  geom_vline(data = summary_gruops, aes(xintercept = value), linetype = "dotted", 
             color = "black", size = 0.4) +
  geom_errorbar(data = summary_condition, aes(xmin= value-sd, xmax= value+sd, 
                                              y = Tile, colour = Condition, 
                                              group = Condition), width=.2,
                position=position_dodge(0.1)) +
  geom_point(data = summary_condition, aes(x = value, y = Tile, colour = Condition, 
                                           group = Condition, shape = Condition), 
             size = 3, position=position_dodge(0.1)) +
  geom_path(data = summary_condition, aes(x = value, y = Tile, group = Condition, 
                                          colour = Condition), linetype = "dashed", size = 0.1) +
  th + ylab("Tile") + xlab("Value") +
  scale_colour_manual(values = pa) +
  #coord_cartesian(xlim = c(1, 3)) +
  facet_wrap(. ~ Metric)

jpeg("spatial_performance.jpeg", quality = 100, res = 600, width = 210, height = 120, units = "mm", pointsize = 12) # JPEG device

plot

dev.off()
