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

#pa <- c("#1b9e77", "#d95f02", "#7570b3")
pa <- c("#481568FF", "#1F968BFF", "#B8DE29FF")
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
# Root path

path <- "F:/TRAINING/level3_lsf-pixels"
path <- "/media/antonio/antonio_ssd/TRAINING/level3_lsf-pixels"

#Plot --------------------------------------------------------------------------

data <- fread(paste0(path, "/master_training.csv"))

data$Condition <- as.factor(data$Condition)
data$Condition <- factor(data$Condition, levels = c("Healthy", "Wilted", "Dead"))

data$year <- as.factor(data$year)
data$year <- factor(data$year, levels = c("2018", "2019", "2021"))


#VSS --------------------------------------

VSS_select <- c("Condition", "year", "VSS")
VSS_frame <- data[, ..VSS_select]
colnames(VSS_frame) <- c("Condition", "Year", "value")
VSS_frame <- VSS_frame[is.infinite(value) != TRUE]
VSS_frame$value <- VSS_frame$value/10000   

#Summary
VSS_summary_gruops <- summarySE(VSS_frame, measurevar = "value",
                                groupvars=c("Condition", "Year"))
  
VSS_summary_condition <- summarySE(VSS_frame, measurevar = "value",
                                   groupvars=c("Condition"))
  
VSS_summary_condition$lower_ci <- VSS_summary_condition$value - VSS_summary_condition$ci
VSS_summary_condition$upper_ci <- VSS_summary_condition$value + VSS_summary_condition$ci
VSS_summary_condition[, 2:8] <- round(VSS_summary_condition[, 2:8], 4)
  


VSS <- ggplot() +
    geom_flat_violin(data = VSS_frame, aes(x = Condition, y = value, fill = Year), position = position_nudge(x = .08, y = 0), adjust = 1.5, trim = FALSE, alpha = .25, colour = "white") +
    geom_point(data = VSS_frame, aes(x = as.numeric(Condition)-.15, y = value, colour = Year, shape = Condition), position = position_jitter(width = .05), size = .25, shape = 20) +
    geom_boxplot(data = VSS_frame, aes(x = Condition, y = value, fill = Year), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
    geom_line(data = VSS_summary_gruops, aes(x = as.numeric(Condition)+.1, y = value, group = Year, colour = Year), linetype = 3) +
    geom_point(data = VSS_summary_gruops, aes(x = as.numeric(Condition)+.1, y = value, group = Year, colour = Year), shape = 18) +
    scale_colour_manual(values = pa) +
    scale_fill_manual(values = pa) +
    th + xlab("") + ylab("VSS") + 
    scale_y_continuous(breaks = c(0, 0.2, 0.4)) +
    coord_cartesian(ylim = c(0, 0.45), xlim = c(0.5, 3.5), expand = FALSE)
  

#PPM --------------------------------------

PPM_select <- c("Condition", "year", "PPM")
PPM_frame <- data[, ..PPM_select]
colnames(PPM_frame) <- c("Condition", "Year", "value")
PPM_frame <- PPM_frame[is.infinite(value) != TRUE]
PPM_frame$value <- PPM_frame$value   
PPM_frame <- PPM_frame[value <= 1.25]

#Summary
PPM_summary_gruops <- summarySE(PPM_frame, measurevar = "value",
                                groupvars=c("Condition", "Year"))

PPM_summary_condition <- summarySE(PPM_frame, measurevar = "value",
                                   groupvars=c("Condition"))

PPM_summary_condition$lower_ci <- PPM_summary_condition$value - PPM_summary_condition$ci
PPM_summary_condition$upper_ci <- PPM_summary_condition$value + PPM_summary_condition$ci
PPM_summary_condition[, 2:8] <- round(PPM_summary_condition[, 2:8], 4)


PPM <- ggplot() +
  geom_flat_violin(data = PPM_frame, aes(x = Condition, y = value, fill = Year), position = position_nudge(x = .08, y = 0), adjust = 1.5, trim = FALSE, alpha = .25, colour = "white") +
  geom_point(data = PPM_frame, aes(x = as.numeric(Condition)-.15, y = value, colour = Year, shape = Condition), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = PPM_frame, aes(x = Condition, y = value, fill = Year), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = PPM_summary_gruops, aes(x = as.numeric(Condition)+.1, y = value, group = Year, colour = Year), linetype = 3) +
  geom_point(data = PPM_summary_gruops, aes(x = as.numeric(Condition)+.1, y = value, group = Year, colour = Year), shape = 18) +
  scale_colour_manual(values = pa) +
  scale_fill_manual(values = pa) +
  th + xlab("") + ylab("PPM") +
  scale_y_continuous(breaks = c(0, 0.4, 0.8)) +
  coord_cartesian(xlim = c(0.5, 3.5), ylim = c(0, 0.85), expand = FALSE)
  

#VCV --------------------------------------

VCV_select <- c("Condition", "year", "VCV")
VCV_frame <- data[, ..VCV_select]
colnames(VCV_frame) <- c("Condition", "Year", "value")
VCV_frame <- VCV_frame[is.infinite(value) != TRUE]
VCV_frame$value <- VCV_frame$value   
VCV_frame <- VCV_frame[value <= 1.25]

#Summary
VCV_summary_gruops <- summarySE(VCV_frame, measurevar = "value",
                                groupvars=c("Condition", "Year"))

VCV_summary_condition <- summarySE(VCV_frame, measurevar = "value",
                                   groupvars=c("Condition"))

VCV_summary_condition$lower_ci <- VCV_summary_condition$value - VCV_summary_condition$ci
VCV_summary_condition$upper_ci <- VCV_summary_condition$value + VCV_summary_condition$ci
VCV_summary_condition[, 2:8] <- round(VCV_summary_condition[, 2:8], 4)


VCV <- ggplot() +
  geom_flat_violin(data = VCV_frame, aes(x = Condition, y = value, fill = Year), position = position_nudge(x = .08, y = 0), adjust = 1.5, trim = FALSE, alpha = .25, colour = "white") +
  geom_point(data = VCV_frame, aes(x = as.numeric(Condition)-.15, y = value, colour = Year, shape = Condition), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = VCV_frame, aes(x = Condition, y = value, fill = Year), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = VCV_summary_gruops, aes(x = as.numeric(Condition)+.1, y = value, group = Year, colour = Year), linetype = 3) +
  geom_point(data = VCV_summary_gruops, aes(x = as.numeric(Condition)+.1, y = value, group = Year, colour = Year), shape = 18) +
  scale_colour_manual(values = pa) +
  scale_fill_manual(values = pa) +
  th + xlab("") + ylab("VCV") +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6)) +
  coord_cartesian(xlim = c(0.5, 3.5), ylim = c(0, 0.60), expand = FALSE)



#IFR --------------------------------------

IFR_select <- c("Condition", "year", "IFR")
IFR_frame <- data[, ..IFR_select]
colnames(IFR_frame) <- c("Condition", "Year", "value")
IFR_frame <- IFR_frame[is.infinite(value) != TRUE]
IFR_frame$value <- IFR_frame$value   
IFR_frame$value <- IFR_frame$value/1000

#Summary
IFR_summary_gruops <- summarySE(IFR_frame, measurevar = "value",
                                groupvars=c("Condition", "Year"))

IFR_summary_condition <- summarySE(IFR_frame, measurevar = "value",
                                   groupvars=c("Condition"))

IFR_summary_condition$lower_ci <- IFR_summary_condition$value - IFR_summary_condition$ci
IFR_summary_condition$upper_ci <- IFR_summary_condition$value + IFR_summary_condition$ci
IFR_summary_condition[, 2:8] <- round(IFR_summary_condition[, 2:8], 4)

IFR <- ggplot() +
  geom_flat_violin(data = IFR_frame, aes(x = Condition, y = value, fill = Year), position = position_nudge(x = .08, y = 0), adjust = 1.5, trim = FALSE, alpha = .25, colour = "white") +
  geom_point(data = IFR_frame, aes(x = as.numeric(Condition)-.15, y = value, colour = Year, shape = Condition), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = IFR_frame, aes(x = Condition, y = value, fill = Year), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = IFR_summary_gruops, aes(x = as.numeric(Condition)+.1, y = value, group = Year, colour = Year), linetype = 3) +
  geom_point(data = IFR_summary_gruops, aes(x = as.numeric(Condition)+.1, y = value, group = Year, colour = Year), shape = 18, alpha = 0.5) +
  scale_colour_manual(values = pa) +
  scale_fill_manual(values = pa) +
  th + xlab("") + ylab("IFR") +
  scale_y_continuous(breaks = c(0, 1.5, 3)) +
  coord_cartesian(xlim = c(0.5, 3.5),  ylim = c(0, 3), expand = FALSE)


#figure
figure <- ggarrange(VSS, IFR, 
                    VCV, PPM, 
                    labels = c("a", "b", "c", "d"),
                    font.label = list(size = 15, color = "black", face = "plain", family = NULL),
                    label.x = 0.15,
                    label.y = 0.99,
                    ncol = 2, nrow = 2,  align = "hv",
                    widths = c(2, 2),
                    heights = c(2, 2),
                    common.legend = TRUE)

jpeg("polar-metrics.jpeg", quality = 100, res = 300, width = 210, height = 150, units = "mm", pointsize = 12) # JPEG device

figure

dev.off()

