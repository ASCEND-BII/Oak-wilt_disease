################################################################################
##### 02 - Data cleaning for developing a model
################################################################################

#' @description A script for cleaning the pixels observations for future model 
#' development.

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(ggridges)
library(ggplot2)

#-------------------------------------------------------------------------------
#Path
path <- "/media/antonio/antonio_ssd/TRAINING/level3_lsf-pixels"

#-------------------------------------------------------------------------------
# Compile observations

files <- list.files(path = path, 
                    pattern = ".txt", 
                    all.files = TRUE,
                    full.names = FALSE, 
                    recursive = TRUE)

#Arrange path in frame
frame <- data.table(matrix(unlist(strsplit(files, "/")), 
                           nrow= length(files), 
                           byrow=TRUE), stringsAsFactors=FALSE)
colnames(frame) <- c("folder", "file")

#Read in a loop
data <- data.table()

for(i in 1:nrow(frame)) {
  
  file <- fread(paste0(path, "/", frame$folder[i], "/", frame$file[i]))
  file$sensor <- frame$folder[i]
  data <- rbind(data, file)

}

#Reshape frame
data <- dcast(data, tile + sensor + ID + x + y + Condition + year ~ metric)
data <- na.exclude(data)

#Export
fwrite(data, paste0(path, "/master_observations.csv"))

#-------------------------------------------------------------------------------
# Match observations for training
#-------------------------------------------------------------------------------
data <- fread(paste0(path, "/master_observations.csv"))

data$Condition <- as.factor(data$Condition)
data$Condition <- factor(data$Condition, levels = c("Healthy", "Wilted", "Dead"))

data$year <- as.factor(data$year)
data$year <- factor(data$year, levels = c("2018", "2019", "2021"))

ggplot(data, aes(year, VGM/VLV, fill = Condition)) + 
  ggbeeswarm::geom_quasirandom(shape = 21, size=2, dodge.width = .75, color="black", alpha=.5, show.legend = F) +
  scale_fill_viridis_d( option = "D")+
  geom_violin(alpha=0.5, position = position_dodge(width = .75), size = 0.4, color=NA) +
  geom_boxplot(outlier.size = -1, color="black", lwd= .4, alpha = 0.7,show.legend = F)+
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(alpha = 1,color="black"))) +
  scale_y_continuous(limits = c(-10, 10))





ggplot(data, aes(Condition, VPS, fill = year)) + 
  ggbeeswarm::geom_quasirandom(shape = 21, size=2, dodge.width = .75, color="black", alpha=.5, show.legend = F) +
  scale_fill_viridis_d( option = "D")+
  geom_violin(alpha=0.5, position = position_dodge(width = .75), size = 0.4, color=NA) +
  geom_boxplot(outlier.size = -1, color="black", lwd= .4, alpha = 0.7,show.legend = F)+
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(alpha = 1,color="black")))

ggplot(data, aes(Condition, VLV, fill = year)) + 
  ggbeeswarm::geom_quasirandom(shape = 21, size=2, dodge.width = .75, color="black", alpha=.5, show.legend = F) +
  scale_fill_viridis_d( option = "D")+
  geom_violin(alpha=0.5, position = position_dodge(width = .75), size = 0.4, color=NA) +
  geom_boxplot(outlier.size = -1, color="black", lwd= .4, alpha = 0.7,show.legend = F)+
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(alpha = 1,color="black")))

ggplot(data, aes(Condition, VMS, fill = year)) + 
  ggbeeswarm::geom_quasirandom(shape = 21, size=2, dodge.width = .75, color="black", alpha=.5, show.legend = F) +
  scale_fill_viridis_d( option = "D")+
  geom_violin(alpha=0.5, position = position_dodge(width = .75), size = 0.4, color=NA) +
  geom_boxplot(outlier.size = -1, color="black", lwd= .4, alpha = 0.7,show.legend = F)+
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(alpha = 1,color="black"))) 
  scale_y_continuous(limits = c(-10, 10))

ggplot(data, aes(Condition, IFR, fill = year)) + 
  ggbeeswarm::geom_quasirandom(shape = 21, size=2, dodge.width = .75, color="black", alpha=.5, show.legend = F) +
  scale_fill_viridis_d( option = "D")+
  geom_violin(alpha=0.5, position = position_dodge(width = .75), size = 0.4, color=NA) +
  geom_boxplot(outlier.size = -1, color="black", lwd= .4, alpha = 0.7,show.legend = F)+
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(alpha = 1,color="black")))
  scale_y_continuous(limits = c(-10, 10))



ggplot(data, aes(Condition, VPS, fill = sensor)) + 
  ggbeeswarm::geom_quasirandom(shape = 21, size=2, dodge.width = .75, color="black", alpha=.5, show.legend = F) +
  scale_fill_viridis_d( option = "D")+
  geom_violin(alpha=0.5, position = position_dodge(width = .75), size = 0.4, color=NA) +
  geom_boxplot(outlier.size = -1, color="black", lwd= .4, alpha = 0.7,show.legend = F)+
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(alpha = 1,color="black")))

ggplot(data, aes(Condition, VSS, fill = sensor)) + 
  ggbeeswarm::geom_quasirandom(shape = 21, size=2, dodge.width = .75, color="black", alpha=.5, show.legend = F) +
  scale_fill_viridis_d( option = "D")+
  geom_violin(alpha=0.5, position = position_dodge(width = .75), size = 0.4, color=NA) +
  geom_boxplot(outlier.size = -1, color="black", lwd= .4, alpha = 0.7,show.legend = F)+
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(alpha = 1,color="black")))


ggplot(data, aes(Condition, VGM, fill = sensor, group = interaction(sensor, Condition))) + 
  geom_jitter(size=0.4, alpha=0.9) +
  geom_boxplot() +
  ylab('LSF') +
  theme_minimal() +
  theme(axis.title.x = element_blank())

ggplot(data, aes(Condition, VAV, fill = sensor, group = interaction(sensor, Condition))) + 
  geom_jitter(size=0.4, alpha=0.9) +
  geom_boxplot() +
  ylab('LSF') +
  theme_minimal() +
  theme(axis.title.x = element_blank())


col1 <- colorRampPalette(brewer.pal(9,"BrBG"))
corrplot(corrmatrix,method = "square", order = "FPC", tl.col = "black", tl.cex = 0.75, p.mat = res1[[1]], sig.level = 0.05, insig = "pch", pch.cex = 1, col = col1(100))