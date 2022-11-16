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
path <- "F:/TRAINING/level3_lsf-pixels"

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
data <- dcast(data, tile + sensor + ID + x + y + YOI + Condition + observation + VI ~ metric)
data[, sensor := strsplit(sensor, "_")[[1]][1], by = seq_along(1:nrow(data))]
data <- na.exclude(data)

#Add unique ID of sample
remove <- na.exclude(data)
unique_IDs <- remove[, .N, by= c("tile", "ID", "Condition", "sensor")]
unique_IDs$N <- 1:nrow(unique_IDs)
data <- merge(unique_IDs, data, by = c("tile", "ID", "Condition", "sensor"), 
              all.x = TRUE, all.y = FALSE)
data <- data[order(N, VI)]

#Export
fwrite(data, paste0(path, "/master_observations.csv"))

#-------------------------------------------------------------------------------
# Match observations for training
#-------------------------------------------------------------------------------
data <- fread(paste0(path, "/master_observations.csv"))

data$Condition <- as.factor(data$Condition)
data$Condition <- factor(data$Condition, levels = c("Healthy", "Wilted", "Dead"))

data$YOI <- as.factor(data$YOI)
data$YOI <- factor(data$YOI, levels = c("2018", "2019", "2021"))

data$current <- data$YOI == data$observation
data <- subset(data, current == TRUE)
data <- subset(data, VI == "CCI")

#Remove 0 values
data[VPS == 0, VPS := NA]
data[VGM == 0, VGM := NA]
data[VGV == 0, VGV := NA]
data[VSS == 0, VSS := NA]
data[IFR == 0, IFR := NA]
data <- na.exclude(data)

#New variables
data$PPM <- (data$VPS/data$VGM)/data$VGM
data$VCV <- data$VGV/data$VGM

#-------------------------------------------------------------------------------

ggplot(data, aes(Condition, (VPS-VGM)/VGM, fill = YOI)) + 
  ggbeeswarm::geom_quasirandom(shape = 21, size=2, dodge.width = .75, color="black", alpha=.5, show.legend = F) +
  scale_fill_viridis_d( option = "D")+
  geom_violin(alpha=0.5, position = position_dodge(width = .75), size = 0.4, color=NA) +
  geom_boxplot(outlier.size = -1, color="black", lwd= .4, alpha = 0.7,show.legend = F)+
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(alpha = 1,color="black"))) +
  coord_cartesian(ylim = c(0, 0.85))

ggplot(data, aes(Condition, VGM, fill = YOI)) + 
  ggbeeswarm::geom_quasirandom(shape = 21, size=2, dodge.width = .75, color="black", alpha=.5, show.legend = F) +
  scale_fill_viridis_d( option = "D") +
  geom_violin(alpha=0.5, position = position_dodge(width = .75), size = 0.4, color=NA) +
  geom_boxplot(outlier.size = -1, color="black", lwd= .4, alpha = 0.7,show.legend = F)+
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(alpha = 1,color="black"))) 

ggplot(data, aes(Condition, VGV/VGM, fill = YOI)) + 
  ggbeeswarm::geom_quasirandom(shape = 21, size=2, dodge.width = .75, color="black", alpha=.5, show.legend = F) +
  scale_fill_viridis_d( option = "D")+
  geom_violin(alpha=0.5, position = position_dodge(width = .75), size = 0.4, color=NA) +
  geom_boxplot(outlier.size = -1, color="black", lwd= .4, alpha = 0.7,show.legend = F)+
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(alpha = 1,color="black"))) +
  coord_cartesian(ylim = c(0, 0.75))

ggplot(data, aes(Condition, VSS, fill = YOI)) + 
  ggbeeswarm::geom_quasirandom(shape = 21, size=2, dodge.width = .75, color="black", alpha=.5, show.legend = F) +
  scale_fill_viridis_d( option = "D")+
  geom_violin(alpha=0.5, position = position_dodge(width = .75), size = 0.4, color=NA) +
  geom_boxplot(outlier.size = -1, color="black", lwd= .4, alpha = 0.7,show.legend = F)+
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(alpha = 1,color="black"))) 

ggplot(data, aes(Condition, IFR, fill = YOI)) + 
  ggbeeswarm::geom_quasirandom(shape = 21, size=2, dodge.width = .75, color="black", alpha=.5, show.legend = F) +
  scale_fill_viridis_d( option = "D")+
  geom_violin(alpha=0.5, position = position_dodge(width = .75), size = 0.4, color=NA) +
  geom_boxplot(outlier.size = -1, color="black", lwd= .4, alpha = 0.7,show.legend = F)+
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(alpha = 1,color="black"))) 

data <- data[, c("tile", "ID", "Condition", "sensor", "N", "x", "y", "YOI", "PPM", "VCV", "VSS", "IFR")]
colnames(data)[8] <- "year"
fwrite(data, paste0(path, "/master_training.csv"))
