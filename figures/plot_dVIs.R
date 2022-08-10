################################################################################
#### Figure for the ternary plot and violin plots. 
################################################################################

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(ggplot2)
library(ggtern)
library(ggpubr)
library(scales)

#-------------------------------------------------------------------------------
# Root path

path <- "/media/antonio/antonio_ssd/FORCE"
path <- "F:/FORCE"

#-------------------------------------------------------------------------------
# Reading 

data <- fread(paste0(path, "/model/data/data_clean.csv"))

#-------------------------------------------------------------------------------
# Data preparation 

#Normalization
data$nCCI <- (data$dCCI-min(data$dCCI))/(max(data$dCCI)-min(data$dCCI))
data$nNDW <- (data$dNDW-min(data$dNDW))/(max(data$dNDW)-min(data$dNDW))
data$nCRE <- (data$dCRE-min(data$dCRE))/(max(data$dCRE)-min(data$dCRE))
data$condition <- as.factor(data$condition)
data$condition <- factor(data$condition, levels = c("healthy", "wilted", "dead"))


#-------------------------------------------------------------------------------
# Plot
plot <- ggtern(data= data, aes(x= nNDW, y= nCCI, z= nCRE)) +
  geom_point(aes(fill = condition, size = area), alpha = 0.5, colour = "grey20", shape = 21) +
  #geom_density_tern(aes(color=..level..),bins=5) +
  theme_rgbw(base_size = 11) +
  labs( x       = expression(paste(Delta, "NDWI"[n], sep = "")),
        xarrow  = expression(paste(Delta, "NDWI"[n], sep = "")),
        y       = expression(paste(Delta, "CCI"[n], sep = "")),
        yarrow  = expression(paste(Delta, "CCI"[n], sep = "")),
        z       = expression(paste(Delta, "CRE"[n], sep = "")),
        zarrow  = expression(paste(Delta, "CRE"[n], sep = ""))) +
  scale_fill_manual(labels = c("Healthy", "Wilted", "Dead"), values = c("#228b22", "#FF0000", "grey40")) +
  guides(fill= guide_legend(title= "Condition"), size = guide_legend(title = expression(paste("Area (m"^2, ")", sep = "")))) +
  theme(legend.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = "white", color = NA))

#Export figure
tiff("figures/Figure_1.tif", width = 16, height = 16, units = "cm", res = 600)

plot

dev.off()

#-------------------------------------------------------------------------------
# Plot

#Plot details
tamano <- 12
tamano2 <- 10
mar <- theme(plot.margin = margin(0, 0, 0, 0, "pt"))
pa <- c("#228b22", "#7a0019", "#777677")
color <- "#d5d6d2"

#Create plots
dCCI <- ggplot(data, aes(x = condition, y = dCCI/10000, fill = condition)) +
  geom_point(shape = 21, size= 1, position = position_jitterdodge(), color="white", alpha= 0.8) +
  geom_violin(alpha=0.4, position = position_dodge(width = .75), size= 0.5, color="black") +
  scale_fill_manual("Condition", values= pa) +
  geom_boxplot(width=0.1, color = "white", alpha = .30, outlier.shape = NA) +
  ylab(expression(paste(Delta, "kCCI"[n], sep = "")))  +
  xlab(NULL)  +
  theme_classic(base_size = tamano) +
  theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.margin = margin(4, 4, 0, 0, "mm")) +
  theme(axis.text.x = element_text(color = "black", size = tamano2), axis.text.y = element_text(color = "black", size = tamano2)) + mar +
  theme(legend.position = "none") +
  scale_x_discrete(name ="", labels = c("","",""))

dNDW <- ggplot(data, aes(x = condition, y = dNDW/10000, fill = condition)) +
  geom_point(shape = 21, size= 1, position = position_jitterdodge(), color="white", alpha= 0.8) +
  geom_violin(alpha=0.4, position = position_dodge(width = .75), size= 0.5, color="black") +
  scale_fill_manual("Condition", values= pa) +
  geom_boxplot(width=0.1, color = "white", alpha = .30, outlier.shape = NA) +
  ylab(expression(paste(Delta, "kNDWI"[n], sep = "")))  +
  xlab(NULL)  +
  theme_classic(base_size = tamano) +
  theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.margin = margin(4, 4, 0, 0, "mm")) +
  theme(axis.text.x = element_text(color = "black", size = tamano2), axis.text.y = element_text(color = "black", size = tamano2)) + mar +
  theme(legend.position = "none") +
  scale_x_discrete(name = "Condition", labels = c("Healthy", "Wilted", "Dead"))

dCRE <- ggplot(data, aes(x = condition, y = dCRE/1000, fill = condition)) +
  geom_point(shape = 21, size= 1, position = position_jitterdodge(), color="white", alpha= 0.8) +
  geom_violin(alpha=0.4, position = position_dodge(width = .75), size= 0.5, color="black") +
  scale_fill_manual("Condition", values= pa) +
  geom_boxplot(width=0.1, color = "white", alpha = .30, outlier.shape = NA) +
  ylab(expression(paste(Delta, "CRE"[n], sep = "")))  +
  xlab(NULL)  +
  theme_classic(base_size = tamano) +
  theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.margin = margin(4, 4, 0, 0, "mm")) +
  theme(axis.text.x = element_text(color = "black", size = tamano2), axis.text.y = element_text(color = "black", size = tamano2)) + mar +
  theme(legend.position = "none") +
  scale_x_discrete(name ="", labels = c("","",""))

#Create histograms
hist_dCCI <- ggplot(data, aes(x= dCCI/10000)) + 
  geom_density(fill= color, colour = "black", alpha = 0.5) +
  coord_flip() + theme_void() + mar

hist_dNDW <- ggplot(data, aes(x= dNDW/10000)) + 
  geom_density(fill= color, colour = "black", alpha = 0.5) +
  coord_flip() + theme_void() + mar

hist_dCRE <- ggplot(data, aes(x= dCRE/1000)) + 
  geom_density(fill= color, colour = "black", alpha = 0.5) +
  coord_flip() + theme_void() + mar


#Merge panels
Figure_2 <- ggarrange(dCRE, hist_dCRE, 
                      dCCI, hist_dCCI, 
                      dNDW, hist_dNDW,
                      ncol = 2, nrow = 3, align = "hv", 
                      widths = c(2, 1), 
                      heights = c(2, 2, 2),
                      labels = c("a", "", "b", "", "c", ""), 
                      font.label = list(size = 14, color = "black", face = "plain", family = NULL),
                      label.x = 0.25,
                      label.y = 0.99,
                      common.legend = FALSE)

#Export figure
tiff("figures/Figure_2.tif", width = 9.5, height = 13, units = "cm", res = 600)

Figure_2 

dev.off()
