
#-------------------------------------------------------------------------------
#Library

library(data.table)
library(ggplot2)
library(ggpubr)

#-------------------------------------------------------------------------------
# Figures


wilted <- fread("data/wilted_ssam.csv")
data <- wilted

sam_mixing_plot <- function(data) {
  
  fa <- data
  fa <- na.exclude(fa)
  fa[, NDBI := (SWIR1 - NIR) / (SWIR1 + NIR)]
  fa[, CIre := (REDEDGE3 / REDEDGE1) - 1]
  fa <- fa[, c(1, 6, 7)]
  
  th <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  plot <- ggplot(fa, aes(x = NDBI, y = CIre, fill = SAM)) +
    geom_point(shape = 21, colour = "white", size = 1.5, alpha = 0.75) +
    scale_fill_viridis(limits = c(0.6, 1)) +
    theme_bw() + th +
    xlab("NDBI ((1610 - 865) / (1610 + 865))") +
    ylab("CIre (783/705 - 1)") +
    scale_x_continuous(limits = c(-0.5, 0.2)) +
    scale_y_continuous(limits = c(0, 6.2)) +
    theme(legend.position= c("top"), legend.direction = "horizontal", legend.background = element_rect(fill = "transparent"), legend.box.background = element_blank()) +
    guides(colour = guide_legend(ncol = 4, nrow = 4),
           fill = guide_colourbar(barwidth = 15, barheight = 0.7, title.position = "top"))
  
  return(plot)
  
}

wilted <- fread("data/wilted_ssam.csv")
wilted_plot <- sam_mixing_plot(wilted)

healty <- fread("data/healty_ssam.csv")
healty_plot <- sam_mixing_plot(healty)

dead <- fread("data/dead_ssam.csv")
dead_plot <- sam_mixing_plot(dead)

export <- ggarrange(healty_plot,
                    wilted_plot,
                    dead_plot,
                    labels = c("Healty", "Wilted", "Dead"),
                    ncol = 3,
                    label.x = 0.6,
                    label.y = 0.95,
                    common.legend = TRUE)

jpeg("ss_sam.jpeg", width = 21, height = 10, units = "cm", res = 600, pointsize = 7)

export

dev.off()

