################################################################################
### Test triangular variation
################################################################################

library(data.table)
library(ggplot2)
library(ggpubr)

data <- fread("data/spectral_library.csv")
data[, c(8:17)] <- data[, c(8:17)]/10000
data$Endmember <- factor(data$Endmember, levels = c("healty", "wilted", "dead", "ground"),
                         labels = c("Healthy", "Wilted", "Dead", "Ground"))


data[, NDVI := (NIR-RED)/(NIR+RED)]
data[, kNDVI := tanh(((NIR-RED)/(NIR+RED))^2)]
data[, NDGI := (RED-GREEN)/(RED+GREEN)]
data[, NDTI := (SWIR1 - SWIR2) / (SWIR1 + SWIR2)]
data[, ARVI := (NIR - (BLUE - RED)) / (NIR + (BLUE - RED))]
data[, SAVI := (NIR - RED) / (NIR + RED + 0.5) * (1 + 0.5)]
data[, SARVI := (NIR - RED - (BLUE - RED)) / (NIR + RED - (BLUE - RED) + 0.5) * (1 + 0.5)]
data[, TCBRIGHT := 0.2043*BLUE + 0.4158*GREEN + 0.5524*RED + 0.5741*NIR + 0.3124*SWIR1 + 0.2303*SWIR2]
data[, TCGREEN := -0.1603*BLUE - 0.2819*GREEN - 0.4934*RED + 0.7940*NIR - 0.0002*SWIR1 - 0.1446*SWIR2]
data[, TCWET := 0.0315*BLUE + 0.2021*GREEN + 0.3102*RED + 0.1594*NIR - 0.6806*SWIR1 - 0.6109*SWIR2]
data[, TCDI := TCBRIGHT - (TCGREEN + TCWET)]
data[, NDBI := (SWIR1 - NIR) / (SWIR1 + NIR)]
data[, NDMI := (NIR - SWIR1) / (NIR + SWIR1)]
data[, MNDWI := (GREEN - SWIR1) / (GREEN + SWIR1)]
data[, CIre := (REDEDGE3 / REDEDGE1) - 1]
data[, kNDMI := tanh(((NIR - SWIR1) / (NIR + SWIR1))^2)]
data[, MSRren := ((NIR / REDEDGE1) - 1) / sqrt((NIR / REDEDGE1) + 1)]
data <- data[kNDVI >= 0.4]

th <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


relationship <- ggplot(data, aes(x = MSRren, y = NDMI, fill = Endmember, colour = Endmember)) +
  geom_point(shape = 21, colour = "grey", size = 2.5) +
  stat_ellipse(type = "norm", level = 0.75, linetype = "dotted") + 
  #scale_fill_manual(values = c("#1b9e77", "#e7298a", "#7570b3", "#a6761d")) +
  #scale_colour_manual(values = c("#1b9e77", "#e7298a", "#7570b3", "#a6761d")) +
  theme_bw() + th

kNDVI_plot <- ggplot(data, aes(x = Endmember, y = kNDVI, fill = Endmember)) +
  geom_violin(colour = "grey", alpha = .75) +
  geom_point(shape = 21, size = 2, position = position_jitter(seed = 1, width = .2)) +
  geom_boxplot(width=0.02, color = "white", alpha = .70, outlier.shape = NA) +
  scale_fill_manual(values = c("#1b9e77", "#e7298a", "#7570b3", "#a6761d")) +
  ylab("kNDVI") + xlab("Endmember") +
  theme_bw() + th

kNDMI_plot <- ggplot(data, aes(x = Endmember, y = kNDMI, fill = Endmember)) +
  geom_violin(colour = "grey", alpha = .75) +
  geom_point(shape = 21, size = 2, position = position_jitter(seed = 1, width = .2)) +
  geom_boxplot(width=0.02, color = "white", alpha = .70, outlier.shape = NA) +
  scale_fill_manual(values = c("#1b9e77", "#e7298a", "#7570b3", "#a6761d")) +
  ylab("kNDMI") + xlab("Endmember") +
  theme_bw() + th

data_melt <- data[, c(4, 8:17)]
data_melt <- melt(data_melt, 
                  id.vars = c("Endmember"),
                  measure.vars = .SD,
                  value.name = "Reflectance",
                  variable.name = "Band")

data_melt[Band == "BLUE", Wavelength := 490]
data_melt[Band == "GREEN", Wavelength := 560]
data_melt[Band == "RED", Wavelength := 665]
data_melt[Band == "REDEDGE1", Wavelength := 705]
data_melt[Band == "REDEDGE2", Wavelength := 740]
data_melt[Band == "REDEDGE3", Wavelength := 783]
data_melt[Band == "BROADNIR", Wavelength := 842]
data_melt[Band == "NIR", Wavelength := 865]
data_melt[Band == "SWIR1", Wavelength := 1610]
data_melt[Band == "SWIR2", Wavelength := 2190]

data_summary <- data_melt[, list(mean = mean(Reflectance),
                                 sd = sd(Reflectance)),
                          by = c("Endmember", "Wavelength")]
data_summary$Wavelength <- as.numeric(data_summary$Wavelength)
data_summary <- na.exclude(data_summary)
data_summary <- data_summary[Endmember != "Ground"]

spectra <- ggplot(data_summary, aes(x = Wavelength, y = mean, fill = Endmember, colour = Endmember)) +
  geom_ribbon(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.10, colour = NA) +
  geom_point(shape = 21, colour = "grey", size = 2.5, alpha = 0.75) +
  geom_line(linetype = "dotted") +
  scale_fill_manual(values = c("#1b9e77", "#e7298a", "#7570b3")) +
  scale_colour_manual(values = c("#1b9e77", "#e7298a", "#7570b3")) +
  xlab("Wavelength (nm)") + ylab("Reflectance") +
  scale_x_continuous(limits = c(480, 2200), expand = c(0,0)) +
  theme_bw() + th


ggsave(plot = spectra, 
       filename = "sentinle2-spectra.jpeg", 
       width = 15,
       height = 8,
       units = "cm",
       dpi = 600,
       device = "jpeg")


yo <- ggarrange(spectra,
                relationship,
                kNDVI_plot, 
                kNDMI_plot, 
                ncol = 2,
                nrow = 2,
                labels = c("a", "b", "c", "d"),
                common.legend = TRUE)

jpeg("Figure_1.jpeg", width = 21, height = 15, units = "cm", res = 600)

yo

dev.off()



relationship <- ggplot(ow_spectra, aes(x = kNDVI, y = kNDMI, fill = fraction)) +
  geom_point(shape = 21, colour = "grey", size = 2.5) +
  scale_fill_viridis() +
  theme_bw() + th






