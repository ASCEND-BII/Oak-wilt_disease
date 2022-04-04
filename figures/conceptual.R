################################################################################
###### Conceptual framework
################################################################################

################################################################################
#' Libraries

library(ggplot2)
library(data.table)


################################################################################
#' Load data

data <- fread("data/point_extraction.csv")
data$condition <- as.factor(data$condition)
data$condition <- factor(data$condition, levels = c("Healthy", "Wilted", "Dead"))

################################################################################
###Plot features----------------------------------------------------------------

th <- theme(
  panel.background = element_rect(fill = "transparent"),
  plot.background = element_rect(fill = "transparent", color = NA),
  panel.spacing = unit(0,"null"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(color = "transparent"))


################################################################################
#' Figure



a <- ggplot(data, aes(x = (time1/10000), y = (time2/10000), fill = condition)) +
  geom_point(shape = 21, size = 2, colour = "white") +
  xlab("NGRDI (early-June)") + ylab("NGRDI (late-August)") +
  scale_x_continuous(limits = c(-0.05, 0.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.05, 0.5), expand = c(0, 0)) +
  scale_fill_manual("Condition", values = c("#1a9850", "#d73027", "grey40")) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  theme_linedraw() + th

jpeg("tNGRDI.jpg", width = 12, height = 8, units = "cm", res = 600, pointsize = 12)

a

dev.off()


b <- ggplot(data, aes(x = condition, y = dkNGRDI, fill = condition)) +
  geom_point(shape = 21, size= 1, position = position_jitterdodge(), color= "white", alpha= 0.8) +
  geom_violin(alpha=0.4, position = position_dodge(width = .75), size= 0.5, color="grey20") +
  scale_fill_manual("Condition", values = c("#1a9850", "#d73027", "grey40")) +
  geom_boxplot(width=0.1, color = "white", alpha = .70, outlier.shape = NA) +
  xlab("Condition") + ylab(expression(paste(Delta, "NGRDI", sep = ""))) +
  theme_linedraw() + th

jpeg("dNGRDI.jpg", width = 12, height = 8, units = "cm", res = 600, pointsize = 12)

b

dev.off()


  
  
