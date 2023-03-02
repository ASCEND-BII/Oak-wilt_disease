################################################################################
##### 03 - Plot of Temporal Polar Metrics
################################################################################

#' @description A script for ploting the temporal comparison of polar metrics.

#-------------------------------------------------------------------------------
#' Libraries

library(data.table)
library(ggplot2)
library(ggpubr)
library(ggridges)
library(grid)

#Layout properties -------------------------------------------------------------
pa <- c("#1b9e77", "#d95f02", "#7570b3")
tamano <- 12
tamano2 <- 11

#-------------------------------------------------------------------------------
# Plot function

plot_LSP <- function(frame, limits, breaks, intercept = -1, label) {
  
  plot <- ggplot(frame, aes(x = value,
                            y = Year,
                            color = Condition,
                            point_color = Condition,
                            fill = Condition)) +
    geom_density_ridges(jittered_points = TRUE, 
                        scale = .90, 
                        rel_min_height = .01,
                        point_shape = "|", 
                        point_size = 2, size = 0.25,
                        position = position_points_jitter(height = 0),
                        alpha = 0.25,
                        quantile_lines = TRUE,
                        quantiles = 2) +
    geom_vline(xintercept= intercept, linetype= "solid", color = "grey70") +
    scale_y_discrete(expand = c(0, 0.05)) +
    ylab("") + xlab(label) +
    scale_x_continuous(limits = limits, expand = c(0, 0), breaks = breaks) +
    scale_fill_manual(values = pa) +
    scale_color_manual(values = pa, guide = "none") +
    scale_discrete_manual("point_color", values = pa, guide = "none") +
    coord_cartesian(xlim = limits, clip = "off") +
    guides(fill = guide_legend(override.aes = list(fill = pa,
                                                   color = NA, 
                                                   point_color = NA))) +   
    theme(plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "grey"),
          plot.margin = margin(4, 4, 4, 4, "mm"))
    
  
  return(plot)
  
}

#-------------------------------------------------------------------------------
# Root path

path <- "/media/antonio/Work/Projects/Oak-wilt_mapping/level3_pixel-extraction"

#Plot Raw observations ---------------------------------------------------------

data <- fread(paste0(path, "/master_training.csv"))
data <- na.exclude(data)

# As condition and dataset factor
data$Condition <- as.factor(data$Condition)
data$Condition <- factor(data$Condition, levels = c("Healthy", "Symptomatic", "Dead"))

data$dataset <- as.factor(data$dataset)
data$dataset <- factor(data$dataset, levels = c("2018", "2019", "2021"))

#VSS --------------------------------------

VSS_select <- c("Condition", "dataset", "VSS")
VSS_frame <- data[, ..VSS_select]
colnames(VSS_frame) <- c("Condition", "Year", "value")
VSS_frame$value <- VSS_frame$value/10000

VSS <- plot_LSP(frame = VSS_frame, 
                limits = c(0, 0.5), 
                breaks = c(0, 0.25, 0.5), 
                label = "VSS")
  
#PPM --------------------------------------

VES_select <- c("Condition", "dataset", "VES")
VES_frame <- data[, ..VES_select]
colnames(VES_frame) <- c("Condition", "Year", "value")
VES_frame$value <- VES_frame$value/10000

VES <- plot_LSP(frame = VES_frame, 
                limits = c(0, 0.5), 
                breaks = c(0, 0.25, 0.5), 
                label = "VES")

#VCV --------------------------------------

VCV_select <- c("Condition", "dataset", "VCV")
VCV_frame <- data[, ..VCV_select]
colnames(VCV_frame) <- c("Condition", "Year", "value")
VCV_frame$value <- VCV_frame$value/1000

VCV <- plot_LSP(frame = VCV_frame, 
                limits = c(0, 0.7), 
                breaks = c(0, 0.3, 0.6), 
                label = "VCV")


#Plot Raw observations ---------------------------------------------------------

data <- fread(paste0(path, "/master_normalized.csv"))
data <- na.exclude(data)

# As condition and dataset factor
data$Condition <- as.factor(data$Condition)
data$Condition <- factor(data$Condition, levels = c("Healthy", "Symptomatic", "Dead"))

data$dataset <- as.factor(data$dataset)
data$dataset <- factor(data$dataset, levels = c("2018", "2019", "2021"))

#VSS --------------------------------------

VSS_select <- c("Condition", "dataset", "VSS")
VSS_frame <- data[, ..VSS_select]
colnames(VSS_frame) <- c("Condition", "Year", "value")

VSS_normalized <- plot_LSP(frame = VSS_frame, 
                           limits = c(-7.5, 7.5), 
                           breaks = c(-5, 0, 5), 
                           intercept = 0,
                           label = expression(VSS[" z-score"]))

#PPM --------------------------------------

VES_select <- c("Condition", "dataset", "VES")
VES_frame <- data[, ..VES_select]
colnames(VES_frame) <- c("Condition", "Year", "value")

VES_normalized <- plot_LSP(frame = VES_frame,
                           limits = c(-4.8, 4.8),
                           breaks = c(-3, 0, 3),
                           intercept = 0,
                           label = expression(VES[" z-score"]))

#VCV --------------------------------------

VCV_select <- c("Condition", "dataset", "VCV")
VCV_frame <- data[, ..VCV_select]
colnames(VCV_frame) <- c("Condition", "Year", "value")

VCV_normalized <- plot_LSP(frame = VCV_frame,
                           limits = c(-7.5, 7.5),
                           breaks = c(-4, 0, 4),
                           intercept = 0,
                           label = expression(VCV[" z-score"]))

#Unite figure -----------------------------------------------------------------
#figure
figure <- ggarrange(VSS, VSS_normalized, 
                    VES, VES_normalized,
                    VCV, VCV_normalized,
                    labels = c("a", "b", "c", "d", "e", "f"),
                    font.label = list(size = 15, color = "black", face = "plain", family = NULL),
                    label.x = 0.20,
                    label.y = 0.97,
                    ncol = 2, nrow = 3,  align = "hv",
                    widths = c(2, 2),
                    heights = c(2, 2),
                    common.legend = TRUE)



jpeg("figures/polar-metrics_temporal.jpeg", quality = 100, res = 300, width = 180, height = 210, units = "mm", pointsize = 11) # JPEG device

annotate_figure(figure, left = textGrob("Year", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Land Surface Phenology", gp = gpar(cex = 1.3)))

dev.off()

