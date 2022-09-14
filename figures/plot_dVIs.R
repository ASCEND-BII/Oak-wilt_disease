################################################################################
#### Figure for the ternary plot and violin plots. 
################################################################################

#-------------------------------------------------------------------------------
# Libraries

library(ggplot2)
library(ggtern)
library(ggpubr)
library(scales)

#-------------------------------------------------------------------------------
# Root path

path <- "/media/antonio/antonio_ssd/FORCE"

#-------------------------------------------------------------------------------
# Reading 

data <- fread(paste0(path, "/model/data/data_clean.csv"))
data <- fread("master.csv")

metrics <- c("VGV", "VGA", "VPA", "RAF")

#-------------------------------------------------------------------------------
# Data preparation 
normalized <- function(y) {
  
  x<-y[!is.na(y)]
  
  x<-(x - min(x)) / (max(x) - min(x))
  
  y[!is.na(y)]<-x
  
  return(y)
}

#Normalization
data$VPA <- (data$VPA-min(data$VPA))/(max(data$VPA)-min(data$VPA))
data$VGV <- (data$VGV-min(data$VGV))/(max(data$VGV)-min(data$VGV))
data$VSS <- (data$VSS-min(data$VSS))/(max(data$VSS)-min(data$VSS))
data$condition <- as.factor(data$condition)
data$condition <- factor(data$condition, levels = c("healthy", "wilted", "dead"))

a <- "VPA"
b <- "VGV"
c <- "IGS"

#-------------------------------------------------------------------------------
# Plot
plot <- ggtern(data= data, aes(x= VPA, y= VGV, z= IGS)) +
  geom_point(aes(fill = condition, size = area), alpha = 0.5, colour = "grey20", shape = 21) +
  #geom_density_tern(aes(color=..level..),bins=5) +
  theme_rgbw(base_size = 11) +
  labs( x       = a,
        xarrow  = a,
        y       = b,
        yarrow  = b,
        z       = c,
        zarrow  = c) +
  scale_fill_manual(labels = c("Healthy", "Wilted", "Dead"), values = c("#228b22", "#FF0000", "grey40")) +
  guides(fill= guide_legend(title= "Condition"), size = guide_legend(title = expression(paste("Area (m"^2, ")", sep = "")))) +
  theme(legend.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = "white", color = NA))

#Export figure
tiff("figures/Figure_1.tif", width = 16, height = 16, units = "cm", res = 600, pointsize = 14)

plot

dev.off()

#-------------------------------------------------------------------------------
# Plot

data <- rbind(condition_training, condition_testing)
data$condition <- factor(data$condition, levels = c("healthy", "wilted", "dead"))

#Plot details
tamano <- 12
tamano2 <- 10
mar <- theme(plot.margin = margin(0, 0, 0, 0, "pt"))
pa <- c("#228b22", "#7a0019", "#777677")
color <- "#d5d6d2"

#Create plots
VSS <- ggplot(data, aes(x = condition, y = VSS/10000, fill = condition)) +
  geom_point(shape = 21, size= 1, position = position_jitterdodge(), color="white", alpha= 0.8) +
  geom_violin(alpha=0.4, position = position_dodge(width = .75), size= 0.5, color="black") +
  scale_fill_manual("Condition", values= pa) +
  geom_boxplot(width=0.1, color = "white", alpha = .30, outlier.shape = NA) +
  ylab("Value of Start of Season")  +
  xlab(NULL)  +
  theme_classic(base_size = tamano) +
  theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.margin = margin(4, 4, 0, 0, "mm")) +
  theme(axis.text.x = element_text(color = "black", size = tamano2), axis.text.y = element_text(color = "black", size = tamano2)) + mar +
  theme(legend.position = "none") +
  scale_x_discrete(name ="", labels = c("","",""))

VPA <- ggplot(data, aes(x = condition, y = VPA/10000, fill = condition)) +
  geom_point(shape = 21, size= 1, position = position_jitterdodge(), color="white", alpha= 0.8) +
  geom_violin(alpha=0.4, position = position_dodge(width = .75), size= 0.5, color="black") +
  scale_fill_manual("Condition", values= pa) +
  geom_boxplot(width=0.1, color = "white", alpha = .30, outlier.shape = NA) +
  ylab("Value of Peak Amplitude")  +
  xlab(NULL)  +
  theme_classic(base_size = tamano) +
  theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.margin = margin(4, 4, 0, 0, "mm")) +
  theme(axis.text.x = element_text(color = "black", size = tamano2), axis.text.y = element_text(color = "black", size = tamano2)) + mar +
  theme(legend.position = "none") +
  scale_x_discrete(name ="", labels = c("","",""))

VGV <- ggplot(data, aes(x = condition, y = VGV/10000, fill = condition)) +
  geom_point(shape = 21, size= 1, position = position_jitterdodge(), color="white", alpha= 0.8) +
  geom_violin(alpha=0.4, position = position_dodge(width = .75), size= 0.5, color="black") +
  scale_fill_manual("Condition", values= pa) +
  geom_boxplot(width=0.1, color = "white", alpha = .30, outlier.shape = NA) +
  ylab("Value of Green Variability")  +
  xlab(NULL)  +
  theme_classic(base_size = tamano) +
  theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.margin = margin(4, 4, 0, 0, "mm")) +
  theme(axis.text.x = element_text(color = "black", size = tamano2), axis.text.y = element_text(color = "black", size = tamano2)) + mar +
  theme(legend.position = "none") +
  scale_x_discrete(name = "Condition", labels = c("Healthy", "Wilted", "Dead"))

IGS <- ggplot(data, aes(x = condition, y = IGS/10000, fill = condition)) +
  geom_point(shape = 21, size= 1, position = position_jitterdodge(), color="white", alpha= 0.8) +
  geom_violin(alpha=0.4, position = position_dodge(width = .75), size= 0.5, color="black") +
  scale_fill_manual("Condition", values= pa) +
  geom_boxplot(width=0.1, color = "white", alpha = .30, outlier.shape = NA) +
  ylab("Integral of Green Season")  +
  xlab(NULL)  +
  theme_classic(base_size = tamano) +
  theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.margin = margin(4, 4, 0, 0, "mm")) +
  theme(axis.text.x = element_text(color = "black", size = tamano2), axis.text.y = element_text(color = "black", size = tamano2)) + mar +
  theme(legend.position = "none") +
  scale_x_discrete(name = "Condition", labels = c("Healthy", "Wilted", "Dead"))


#Merge panels
Figure_2 <- ggarrange(VSS, VPA, 
                      VGV, IGS, 
                      ncol = 2, nrow = 2, align = "hv", 
                      widths = c(2, 2), 
                      heights = c(2, 2),
                      labels = c("a", "b", "c", "d"), 
                      font.label = list(size = 14, color = "black", face = "plain", family = NULL),
                      label.x = 0.15,
                      label.y = 0.99,
                      common.legend = FALSE)

#Export figure
tiff("figures/Figure_2new.tif", width = 20, height = 15, units = "cm", res = 600)

Figure_2 

dev.off()
