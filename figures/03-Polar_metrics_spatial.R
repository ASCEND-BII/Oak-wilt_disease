################################################################################
##### 03 - Plot of Polar Metrics Spatial
################################################################################

#' @description A script for ploting the comparison of polar metrics spatially

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

plot_LSP <- function(frame, limits, breaks, intercept = NULL, label) {
  
  plot <- ggplot(frame, aes(x = value,
                            y = Tile,
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
    geom_vline(xintercept= intercept, linetype= "dotted", color = "grey25") +
    scale_y_discrete(expand = c(0, 0.03)) +
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

# As condition and dataset factor
data$Condition <- as.factor(data$Condition)
data$Condition <- factor(data$Condition, levels = c("Healthy", "Symptomatic", "Dead"))

data$dataset <- as.factor(data$dataset)
data$dataset <- factor(data$dataset, levels = c("2018", "2019", "2021"))

data$tile <- as.factor(data$tile)
data$tile <- factor(data$tile, levels = rev(c("X0014_Y0024", "X0015_Y0024", 
                                              "X0016_Y0024", "X0017_Y0024", 
                                              "X0016_Y0025", "X0017_Y0026", 
                                              "X0016_Y0027", "X0017_Y0027")))


#Get just the 2019 data
data <- subset(data, dataset == "2019")

#VSS --------------------------------------

VSS_select <- c("Condition", "tile", "VSS")
VSS_frame <- data[, ..VSS_select]
colnames(VSS_frame) <- c("Condition", "Tile", "value")
VSS_frame$value <- VSS_frame$value/10000

VSS <- plot_LSP(frame = VSS_frame, 
                limits = c(0, 0.5), 
                breaks = c(0, 0.25, 0.5), 
                label = "VSS")

#VES --------------------------------------

VES_select <- c("Condition", "tile", "VES")
VES_frame <- data[, ..VES_select]
colnames(VES_frame) <- c("Condition", "Tile", "value")
VES_frame$value <- VES_frame$value/10000

VES <- plot_LSP(frame = VES_frame, 
                limits = c(0, 0.5), 
                breaks = c(0, 0.25, 0.5), 
                label = "VES")

#VCV --------------------------------------

VCV_select <- c("Condition", "tile", "VCV")
VCV_frame <- data[, ..VCV_select]
colnames(VCV_frame) <- c("Condition", "Tile", "value")
VCV_frame$value <- VCV_frame$value/1000

VCV <- plot_LSP(frame = VCV_frame, 
                limits = c(0, 0.7), 
                breaks = c(0, 0.3, 0.6), 
                label = "VCV")


#Plot Raw observations ---------------------------------------------------------

data <- fread(paste0(path, "/master_normalized.csv"))

# As condition and dataset factor
data$Condition <- as.factor(data$Condition)
data$Condition <- factor(data$Condition, levels = c("Healthy", "Symptomatic", "Dead"))

data$dataset <- as.factor(data$dataset)
data$dataset <- factor(data$dataset, levels = c("2018", "2019", "2021"))

data$tile <- as.factor(data$tile)
data$tile <- factor(data$tile, levels = rev(c("X0014_Y0024", "X0015_Y0024", 
                                              "X0016_Y0024", "X0017_Y0024", 
                                              "X0016_Y0025", "X0017_Y0026", 
                                              "X0016_Y0027", "X0017_Y0027")))


#Get just the 2019 data
data <- subset(data, dataset == "2019")

#VSS --------------------------------------
VSS_select <- c("Condition", "tile", "VSS")
VSS_frame <- data[, ..VSS_select]
colnames(VSS_frame) <- c("Condition", "Tile", "value")

VSS_normalized <- plot_LSP(frame = VSS_frame, 
                           limits = c(-7.5, 7.5), 
                           breaks = c(-5, 0, 5), 
                           intercept = 0,
                           label = expression(VSS[" z-score"]))

#PPM --------------------------------------

VES_select <- c("Condition", "tile", "VES")
VES_frame <- data[, ..VES_select]
colnames(VES_frame) <- c("Condition", "Tile", "value")

VES_normalized <- plot_LSP(frame = VES_frame,
                           limits = c(-4.8, 4.8),
                           breaks = c(-3, 0, 3),
                           intercept = 0,
                           label = expression(VES[" z-score"]))

#VCV --------------------------------------

VCV_select <- c("Condition", "tile", "VCV")
VCV_frame <- data[, ..VCV_select]
colnames(VCV_frame) <- c("Condition", "Tile", "value")

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
                    label.x = 0.30,
                    label.y = 0.97,
                    ncol = 2, nrow = 3,  align = "hv",
                    widths = c(2, 2),
                    heights = c(2, 2),
                    common.legend = TRUE)

jpeg("figures/polar-metrics_spatial.jpeg", quality = 100, res = 300, width = 210, height = 250, units = "mm", pointsize = 11) # JPEG device

annotate_figure(figure, left = textGrob("Tile", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Land Surface Phenology", gp = gpar(cex = 1.3)))

dev.off()

