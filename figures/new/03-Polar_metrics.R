################################################################################
##### 03 - Plot of Polar Metrics
################################################################################

#' @description A script for ploting the comparison of polar metrics.

#-------------------------------------------------------------------------------
#' Libraries

library(data.table)
library(ggplot2)
library(ggpubr)
library(PupillometryR)
library(Rmisc)

#Layout properties -------------------------------------------------------------
pa <- c("#1b9e77", "#d95f02", "#7570b3")
tamano <- 12
tamano2 <- 11

th <- theme_bw(base_size = tamano) + theme(plot.background = element_blank(),
                                           panel.grid.major = element_blank(),
                                           panel.grid.minor = element_blank(),
                                           plot.margin = margin(4, 12, 0, 4, "pt"),
                                           axis.text.x = element_text(color = "black", size = tamano2),
                                           axis.text.y = element_text(color = "black", size = tamano2),
                                           strip.text.x = element_text(size = tamano, color = "black"),
                                           strip.text.y = element_text(size = tamano, color = "black"),
                                           strip.background = element_rect(color= "black", fill="grey90", linetype="solid"))

#-------------------------------------------------------------------------------
# Plot function

plot_LSP <- function(frame, summary_gruops, limits, breaks, intercept = -1, label) {
  
  plot <- ggplot() +
    geom_hline(yintercept= intercept, linetype= "dashed", color = "grey70") +
    geom_flat_violin(data = frame, aes(x = Year, y = value, fill = Condition), position = position_nudge(x = .08, y = 0), adjust = 1, trim = FALSE, alpha = .25, colour = "white") +
    #geom_point(data = frame, aes(x = as.numeric(Year)-.15, y = value, colour = Condition), position = position_jitter(width = .05), size = 0.1, shape = 20) +
    geom_boxplot(data = frame, aes(x = Year, y = value, fill = Condition), outlier.shape = NA, alpha = .5, width = .13, colour = "black") +
    geom_line(data = summary_gruops, aes(x = as.numeric(Year)+.1, y = value, group = Condition, colour = Condition), linetype = 3) +
    geom_point(data = summary_gruops, aes(x = as.numeric(Year)+.1, y = value, group = Condition, fill = Condition), color = "white", shape = 21,  size = 1.5) +
    scale_colour_manual(values = pa) +
    scale_fill_manual(values = pa) +
    th + xlab("") + ylab(label) +
    scale_y_continuous(breaks = breaks) +
    coord_flip(xlim = c(0.85, 3.5), ylim = limits, expand = FALSE)
  
  return(plot)
  
}

plot_LSP(frame = VGV_frame,
         summary_gruops = VGV_summary_gruops,
         limits = c(-3.8, 3.8),
         breaks = c(-2.5, 0, 2.5),
         intercept = 0,
         label = expression(VGV[" z-score"]))

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

#Summary
VSS_summary_gruops <- summarySE(VSS_frame, measurevar = "value",
                                groupvars=c("Condition", "Year"))
  
VSS_summary_condition <- summarySE(VSS_frame, measurevar = "value",
                                   groupvars=c("Condition"))
  
VSS_summary_condition$lower_ci <- VSS_summary_condition$value - VSS_summary_condition$ci
VSS_summary_condition$upper_ci <- VSS_summary_condition$value + VSS_summary_condition$ci
VSS_summary_condition[, 2:8] <- round(VSS_summary_condition[, 2:8], 4)

VSS <- plot_LSP(frame = VSS_frame, 
                summary_gruops = VSS_summary_gruops, 
                limits = c(0, 0.45), 
                breaks = c(0, 0.2, 0.4), 
                label = "VSS")
  
#PPM --------------------------------------

VGV_select <- c("Condition", "dataset", "VGV")
VGV_frame <- data[, ..VGV_select]
colnames(VGV_frame) <- c("Condition", "Year", "value")
VGV_frame$value <- VGV_frame$value/10000

#Summary
VGV_summary_gruops <- summarySE(VGV_frame, measurevar = "value",
                                groupvars=c("Condition", "Year"))

VGV_summary_condition <- summarySE(VGV_frame, measurevar = "value",
                                   groupvars=c("Condition"))

VGV_summary_condition$lower_ci <- VGV_summary_condition$value - VGV_summary_condition$ci
VGV_summary_condition$upper_ci <- VGV_summary_condition$value + VGV_summary_condition$ci
VGV_summary_condition[, 2:8] <- round(VGV_summary_condition[, 2:8], 4)

VGV <- plot_LSP(frame = VGV_frame, 
                summary_gruops = VGV_summary_gruops, 
                limits = c(0, 0.1), 
                breaks = c(0, 0.05, 0.1), 
                label = "VGV")
  

#VCV --------------------------------------

VCV_select <- c("Condition", "dataset", "VCV")
VCV_frame <- data[, ..VCV_select]
colnames(VCV_frame) <- c("Condition", "Year", "value")
VCV_frame$value <- VCV_frame$value/10000

#Summary
VCV_summary_gruops <- summarySE(VCV_frame, measurevar = "value",
                                groupvars=c("Condition", "Year"))

VCV_summary_condition <- summarySE(VCV_frame, measurevar = "value",
                                   groupvars=c("Condition"))

VCV_summary_condition$lower_ci <- VCV_summary_condition$value - VCV_summary_condition$ci
VCV_summary_condition$upper_ci <- VCV_summary_condition$value + VCV_summary_condition$ci
VCV_summary_condition[, 2:8] <- round(VCV_summary_condition[, 2:8], 4)

VCV <- plot_LSP(frame = VCV_frame, 
                summary_gruops = VCV_summary_gruops, 
                limits = c(0, 0.06), 
                breaks = c(0, 0.03, 0.06), 
                label = "VCV")


#Plot Raw observations ---------------------------------------------------------

data <- fread(paste0(path, "/master_training_normalized.csv"))
data <- na.exclude(data)

# As condition and dataset factor
data$Condition <- as.factor(data$Condition)
data$Condition <- factor(data$Condition, levels = c("Healthy", "Symptomatic", "Dead"))

data$dataset <- as.factor(data$dataset)
data$dataset <- factor(data$dataset, levels = c("2018", "2019", "2021"))

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

data[, VSS := remove_outliers(VSS), by = c("Condition", "dataset")]
data[, VGV := remove_outliers(VGV), by = c("Condition", "dataset")]
data[, VCV := remove_outliers(VCV), by = c("Condition", "dataset")]
data <- na.exclude(data)

#VSS --------------------------------------

VSS_select <- c("Condition", "dataset", "VSS")
VSS_frame <- data[, ..VSS_select]
colnames(VSS_frame) <- c("Condition", "Year", "value")

#Summary
VSS_summary_gruops <- summarySE(VSS_frame, measurevar = "value",
                                groupvars=c("Condition", "Year"))

VSS_summary_condition <- summarySE(VSS_frame, measurevar = "value",
                                   groupvars=c("Condition"))

VSS_summary_condition$lower_ci <- VSS_summary_condition$value - VSS_summary_condition$ci
VSS_summary_condition$upper_ci <- VSS_summary_condition$value + VSS_summary_condition$ci
VSS_summary_condition[, 2:8] <- round(VSS_summary_condition[, 2:8], 4)

VSS_normalized <- plot_LSP(frame = VSS_frame, 
                           summary_gruops = VSS_summary_gruops, 
                           limits = c(-2.4, 2.4), 
                           breaks = c(-2.0, 0, 2.0), 
                           intercept = 0,
                           label = expression(VSS[" z-score"]))

#PPM --------------------------------------

VGV_select <- c("Condition", "dataset", "VGV")
VGV_frame <- data[, ..VGV_select]
colnames(VGV_frame) <- c("Condition", "Year", "value")

#Summary
VGV_summary_gruops <- summarySE(VGV_frame, measurevar = "value",
                                groupvars=c("Condition", "Year"))

VGV_summary_condition <- summarySE(VGV_frame, measurevar = "value",
                                   groupvars=c("Condition"))

VGV_summary_condition$lower_ci <- VGV_summary_condition$value - VGV_summary_condition$ci
VGV_summary_condition$upper_ci <- VGV_summary_condition$value + VGV_summary_condition$ci
VGV_summary_condition[, 2:8] <- round(VGV_summary_condition[, 2:8], 4)

VGV_normalized <- plot_LSP(frame = VGV_frame,
                           summary_gruops = VGV_summary_gruops,
                           limits = c(-3.8, 3.8),
                           breaks = c(-2.5, 0, 2.5),
                           intercept = 0,
                           label = expression(VGV[" z-score"]))

#VCV --------------------------------------

VCV_select <- c("Condition", "dataset", "VCV")
VCV_frame <- data[, ..VCV_select]
colnames(VCV_frame) <- c("Condition", "Year", "value")

#Summary
VCV_summary_gruops <- summarySE(VCV_frame, measurevar = "value",
                                groupvars=c("Condition", "Year"))

VCV_summary_condition <- summarySE(VCV_frame, measurevar = "value",
                                   groupvars=c("Condition"))

VCV_summary_condition$lower_ci <- VCV_summary_condition$value - VCV_summary_condition$ci
VCV_summary_condition$upper_ci <- VCV_summary_condition$value + VCV_summary_condition$ci
VCV_summary_condition[, 2:8] <- round(VCV_summary_condition[, 2:8], 4)

VCV_normalized <- plot_LSP(frame = VCV_frame,
                           summary_gruops = VCV_summary_gruops,
                           limits = c(-3.2, 3.2),
                           breaks = c(-2.5, 0, 2.5),
                           intercept = 0,
                           label = expression(VCV[" z-score"]))

#Unite figure -----------------------------------------------------------------


#figure
figure <- ggarrange(VSS, VSS_normalized, 
                    VGV, VGV_normalized,
                    VCV, VCV_normalized,
                    labels = c("a", "b", "c", "d", "e", "f"),
                    font.label = list(size = 15, color = "black", face = "plain", family = NULL),
                    label.x = 0.20,
                    label.y = 0.97,
                    ncol = 2, nrow = 3,  align = "hv",
                    widths = c(2, 2),
                    heights = c(2, 2),
                    common.legend = TRUE)



jpeg("figures/polar-metrics.jpeg", quality = 100, res = 300, width = 180, height = 210, units = "mm", pointsize = 11) # JPEG device

annotate_figure(figure, left = textGrob("Year", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Land Surface Phenology", gp = gpar(cex = 1.3)))

dev.off()

