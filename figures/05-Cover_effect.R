################################################################################
##### 04 - Plot of pixel coverage and crown area effects 
################################################################################

#' @description A script for ploting the effect of pixel coverage and crown area 
#' of the performance metrics.

#-------------------------------------------------------------------------------
#' Libraries

library(data.table)
library(ggplot2)
library(ggpubr)
library(ggpmisc)

#-------------------------------------------------------------------------------
# Layout properties

pa <- c("#1b9e77", "#d95f02", "#7570b3")
tamano <- 12
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

data <- fread("/media/antonio/Work/Projects/Oak-wilt_mapping/R/data/models/predictions_polygon.csv")


data$Predicted <- as.factor(data$Predicted)
data$Predicted <- factor(data$Predicted, levels = c("Healthy", "Symptomatic", "Dead"))


# ------------------------------------------------------------------------------
# Plot crown effect


plot1 <- ggplot(data = data, aes(x = fraction, y = probability/10000, fill = Predicted, colour = Predicted)) +
  geom_point(shape = 21, colour = "white", alpha = 0.6) +
  geom_smooth(method = "lm", alpha = 0.2, fullrange = TRUE) +
  scale_colour_manual("Conditions predicted", values = pa) +
  scale_fill_manual("Conditions predicted", values = pa) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  xlab("Pixel coverage") + ylab(" ") + 
  th

plot2 <- ggplot(data = data, aes(x = area, y = probability/10000, fill = Predicted, colour = Predicted)) +
  geom_point(shape = 21, colour = "white", alpha = 0.6) +
  geom_smooth(method = "lm", alpha = 0.2, fullrange = TRUE) +
  scale_colour_manual("Conditions predicted", values = pa) +
  scale_fill_manual("Conditions predicted", values = pa) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 210), expand = c(0, 0)) +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  xlab(expression("Crown area ("~m^2~")")) + ylab("Probability") + 
  th


plot <- ggarrange(plot2, plot1, nrow = 1, common.legend = TRUE)

jpeg("figures/fraction-area.jpeg", quality = 100, res = 600, width = 210, 
     height = 100, units = "mm", pointsize = 12) # JPEG device

plot

dev.off()
