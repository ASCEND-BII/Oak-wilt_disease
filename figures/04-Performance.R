################################################################################
##### 04 - Plot of spatial and temporal performance
################################################################################

#' @description A script for ploting the performance of the metrics.

#-------------------------------------------------------------------------------
#' Libraries

library(data.table)
library(ggplot2)
library(ggpubr)
library(Rmisc)

#-------------------------------------------------------------------------------
# Layout properties

pa <- c("#1b9e77", "#d95f02", "#7570b3")
tamano <- 14
tamano2 <- 12

th <- theme_bw(base_size = tamano) + theme(plot.background = element_blank(),
                                           panel.grid.major = element_blank(),
                                           panel.grid.minor = element_blank(),
                                           plot.margin = margin(4, 6, 0, 0, "pt"),
                                           axis.text.x = element_text(color = "black", 
                                                                      size = tamano2),
                                           axis.text.y = element_text(color = "black", 
                                                                      size = tamano2),
                                           strip.text.x = element_text(size = tamano, 
                                                                       color = "black"),
                                           strip.text.y = element_text(size = tamano, 
                                                                       color = "black"),
                                           strip.background = element_rect(color= "black", 
                                                                           fill="grey90", 
                                                                           linetype="solid"))

#-------------------------------------------------------------------------------
# Read data

data <- fread("data/models/byClass.csv")

data$Class <- as.factor(data$Class)
data$Class <- factor(data$Class, levels = c("Healthy", "Symptomatic", "Dead"))

data$Year <- as.factor(data$Year)
data$Year <- factor(data$Year, levels = c("2018", "2019", "2021"))

data$Tile <- as.factor(data$Tile)
data$Tile <- factor(data$Tile, levels = rev(c("All", "X0014_Y0024", "X0015_Y0024", 
                                              "X0016_Y0024", "X0017_Y0024", 
                                              "X0016_Y0025", "X0017_Y0026", 
                                              "X0016_Y0027", "X0017_Y0027")))

# ------------------------------------------------------------------------------
# Spatial variation 2019 

#Select between training and testing
data_spatial <- data[Type == "Testing"]

#Select year
data_spatial <- data_spatial[Year == "2019"]

#Select tile
data_spatial <- data_spatial[Tile != "All"]

#Columns of interest
data_spatial <- data_spatial[, c("Tile", "Class", "Balanced Accuracy",
                                 "Sensitivity", "Specificity", "F1")]

#Melt observations
data_spatial <- melt(data_spatial, id.vars = c("Tile", "Class"),
                      measure.vars = c("Balanced Accuracy", 
                                       "Sensitivity", 
                                       "Specificity", 
                                       "F1"),
                      variable.name = "Metric",
                      value.name = "value")
colnames(data_spatial)[2] <- "Condition"

data_spatial  <- data_spatial[order(Tile, Condition, Metric)]

#Summary
summary_gruops <- summarySE(data_spatial, 
                            measurevar = "value", 
                            groupvars=c("Condition", "Metric"))

summary_condition <- summarySE(data_spatial, 
                               measurevar = "value", 
                               groupvars=c("Tile",
                                           "Condition",
                                           "Metric"))

# ------------------------------------------------------------------------------
# Plot spatial variation

plot <- ggplot() +
  geom_vline(data = summary_gruops, 
             aes(xintercept = value, colour = Condition), 
             linetype = "dashed",
             size = 0.4,
             show.legend = FALSE) +
  geom_errorbar(data = summary_condition, aes(xmin= value-sd, xmax= value+sd, 
                                              y = Tile, colour = Condition, 
                                              group = Condition), width=.2,
                position=position_dodge(0.1)) +
  geom_point(data = summary_condition, aes(x = value, y = Tile, colour = Condition, 
                                           group = Condition, shape = Condition), 
             size = 3, position=position_dodge(0.1)) +
  #geom_path(data = summary_condition, aes(x = value, y = Tile, group = Condition, 
  #                                        colour = Condition), linetype = "dashed", size = 0.1) +
  th + ylab("Tile") + xlab("Value") +
  scale_colour_manual(values = pa) +
  scale_x_continuous(expand = c(0, 0.01)) +
  theme(legend.position="top") +
  #coord_cartesian(xlim = c(1, 3)) +
  facet_wrap(. ~ Metric, scales = "free_x")

jpeg("figures/spatial_performance.jpeg", quality = 100, res = 600, width = 210, 
     height = 150, units = "mm", pointsize = 12) # JPEG device

plot

dev.off()

#-------------------------------------------------------------------------------
# Temporal variation 

#Select between training and testing
data_temporal <- data[Type == "Testing"]

#Select tile
data_temporal <- data[Tile == "All"]

#Columns of interest
data_temporal <- data_temporal[, c("Year", "Class", "Balanced Accuracy", 
                                   "Sensitivity", "Specificity", "F1")]

#Melt observations
data_temporal <- melt(data_temporal, id.vars = c("Year", "Class"),
                      measure.vars = c("Balanced Accuracy", 
                                       "Sensitivity", 
                                       "Specificity", 
                                       "F1"),
                      variable.name = "Metric",
                      value.name = "value")

colnames(data_temporal)[2] <- "Condition"

data_temporal <- data_temporal[order(Year, Condition, Metric)]

#Summary regardless groups
summary_gruops <- summarySE(data_temporal, 
                            measurevar = "value", 
                            groupvars=c("Condition", "Metric"))

#Summary by conditions
summary_condition <- summarySE(data_temporal, 
                               measurevar = "value", 
                               groupvars=c("Year",
                                           "Condition",
                                           "Metric"))
summary_condition$Year <- as.factor(summary_condition$Year)
summary_condition$Year <- factor(summary_condition$Year, levels = c("2018", "2019", "2021"))

# ------------------------------------------------------------------------------
# Plot temporal variation

df <- data.table(Year = c("2018", "2019", "2021"), 
                    X = c("2018", "2019", "2021"),
                    group = c("1", "2", "1"))

plot <- ggplot() +
  geom_vline(data = summary_gruops, 
             aes(xintercept = value, colour = Condition), 
             linetype = "dashed",
             size = 0.4,
             show.legend = FALSE) +
  geom_errorbar(data = summary_condition, 
                aes(xmin= value-sd, 
                    xmax= value+sd, 
                    y = Year, 
                    colour = Condition,
                    group = Condition), 
                width= 0.2,
                position=position_dodge(0.1)) +
  geom_point(data = summary_condition, 
             aes(x = value, 
                 y = Year, 
                 colour = Condition,
                 group = Condition, 
                 shape = Condition),
             size = 3, 
             position=position_dodge(0.1)) +
  geom_path(data = summary_condition, 
            aes(x = value, 
                y = Year, 
                group = Condition, 
                colour = Condition), 
            linetype = "dotted", 
            size = 0.1) +
  th + xlab("Value") + ylab("Year") +
  scale_colour_manual(values = pa) +
  theme(legend.position="top") +
  scale_x_continuous(expand = c(0, 0.015)) +
  facet_wrap(. ~ Metric, scales = "free_x")

jpeg("figures/temporal_performance.jpeg", quality = 100, res = 600, width = 210, 
     height = 150, units = "mm", pointsize = 12) # JPEG device

plot

dev.off()
