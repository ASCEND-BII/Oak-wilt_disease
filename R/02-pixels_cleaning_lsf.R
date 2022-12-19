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
path <- "/media/antonio/Work/Projects/Oak-wilt_mapping/level3_pixel-extraction"
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
  file$dataset <- frame$folder[i]
  data <- rbind(data, file)

}

data <- na.exclude(data)

#Normalize and reshape ---------------------------------------------------------

#Normalized values
data_normalized <- data
data_normalized$target_normalized <- (data_normalized$target_year_value-data_normalized$target_year_mean)/data_normalized$target_year_sd
data_normalized$previus_normalized <- (data_normalized$previus_year_value-data_normalized$previous_year_mean)/data_normalized$previous_year_sd
data_normalized$difference_normalized <- data_normalized$target_normalized - data_normalized$previus_normalized

#Reshape frame
#data <- dcast(data, tile + sensor + ID + x + y + year + Condition ~ metric)
data <- dcast(data, tile + sensor + ID + x + y + date + Condition + VI ~ metric)
data[, sensor := strsplit(sensor, "_")[[1]][1], by = seq_along(1:nrow(data))]

data_normalized <- dcast(data_normalized, tile + sensor + ID + x + y + date + Condition + VI ~ metric)
data_normalized[, sensor := strsplit(sensor, "_")[[1]][1], by = seq_along(1:nrow(data_normalized))]

#Add unique ID of sample
remove <- data
unique_IDs <- remove[, .N, by= c("tile", "ID", "Condition", "sensor")]
unique_IDs$N <- 1:nrow(unique_IDs)
data <- merge(unique_IDs, data, by = c("tile", "ID", "Condition", "sensor"), 
              all.x = TRUE, all.y = FALSE)
data_normalized <- merge(unique_IDs, data_normalized, by = c("tile", "ID", "Condition", "sensor"), 
              all.x = TRUE, all.y = FALSE)

data <- data[order(N, VI)]
data_normalized <- data_normalized[order(N, VI)]

#Export
fwrite(data, paste0(path, "/master_raw.csv"))
fwrite(data_normalized, paste0(path, "/master_normalized.csv"))

#-------------------------------------------------------------------------------
# Match observations for training
#-------------------------------------------------------------------------------
data <- fread(paste0(path, "/master_raw.csv"))
data_normalized <- fread(paste0(path, "/master_normalized.csv"))

data$Condition <- as.factor(data$Condition)
data$Condition <- factor(data$Condition, levels = c("Healthy", "Wilted", "Dead"))

data_normalized$Condition <- as.factor(data_normalized$Condition)
data_normalized$Condition <- factor(data_normalized$Condition, levels = c("Healthy", "Wilted", "Dead"))

data$date <- as.factor(data$date)
data$date <- factor(data$date, levels = c("2018", "2019", "2021"))

data_normalized$date <- as.factor(data_normalized$date)
data_normalized$date <- factor(data_normalized$date, levels = c("2018", "2019", "2021"))

#
#data$year <- as.factor(data$year)
#data$year <- factor(data$year, levels = c("2018", "2019", "2021"))

#data$current <- data$year == data$observation
#data <- subset(data, current == TRUE)

#Remove 0 values
data[VPS == 0, VPS := NA]
data[VGM == 0, VGM := NA]
data[VGV == 0, VGV := NA]
data[VSS == 0, VSS := NA]
data[IFR == 0, IFR := NA]

#New variables
data$PPM <- (data$VPS-data$VGM)/data$VGM
data$VCV <- data$VGV/data$VGM

#-------------------------------------------------------------------------------

ggplot(data_normalized[VI == "KNV"], aes(date, VPS, fill = Condition)) + 
  ggbeeswarm::geom_quasirandom(shape = 21, size=2, dodge.width = .75, color="black", alpha=.5, show.legend = F) +
  scale_fill_viridis_d( option = "D")+
  geom_violin(alpha=0.5, position = position_dodge(width = .75), size = 0.4, color=NA) +
  geom_boxplot(outlier.size = -1, color="black", lwd= .4, alpha = 0.7,show.legend = F)+
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(alpha = 1,color="black"))) 
  #coord_cartesian(ylim = c(0, 0.85))

ggplot(data[VI == "CCI"], aes(date, VGV, fill = Condition)) + 
  ggbeeswarm::geom_quasirandom(shape = 21, size=2, dodge.width = .75, color="black", alpha=.5, show.legend = F) +
  scale_fill_viridis_d( option = "D")+
  geom_violin(alpha=0.5, position = position_dodge(width = .75), size = 0.4, color=NA) +
  geom_boxplot(outlier.size = -1, color="black", lwd= .4, alpha = 0.7,show.legend = F)+
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(alpha = 1,color="black"))) 

ggplot(data[VI == "CCI"], aes(date, (VPS-VGM)/VPS, fill = Condition)) + 
  ggbeeswarm::geom_quasirandom(shape = 21, size=2, dodge.width = .75, color="black", alpha=.5, show.legend = F) +
  scale_fill_viridis_d( option = "D")+
  geom_violin(alpha=0.5, position = position_dodge(width = .75), size = 0.4, color=NA) +
  geom_boxplot(outlier.size = -1, color="black", lwd= .4, alpha = 0.7,show.legend = F)+
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(alpha = 1,color="black"))) +
  coord_cartesian(ylim = c(0, 1))

ggplot(data[VI == "CCI"], aes(date, VPS, fill = Condition)) + 
  ggbeeswarm::geom_quasirandom(shape = 21, size=2, dodge.width = .75, color="black", alpha=.5, show.legend = F) +
  scale_fill_viridis_d( option = "D")+
  geom_violin(alpha=0.5, position = position_dodge(width = .75), size = 0.4, color=NA) +
  geom_boxplot(outlier.size = -1, color="black", lwd= .4, alpha = 0.7,show.legend = F)+
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(alpha = 1,color="black")))

data <- na.exclude(data)

(VPS-VGM)/VPS

unique_IDs 
CCI <- subset(data, VI == "CCI")
CCI$PPM <- (CCI$VPS-CCI$VGM)/CCI$VPS
CCI <- CCI[, c("tile", "ID", "Condition", "sensor", "N", "x", "y", "date","VPS", "VGV", "PPM")]
KNV <- subset(data_normalized, VI == "KNV")
KNV <- KNV[, c("N", "VPS")]
colnames(KNV)[2] <- "VPSn"
CCI <- merge(CCI, KNV, by = c("N"))

CCI <- na.exclude(CCI)

colnames(CCI)[8] <- "year"
fwrite(CCI, paste0(path, "/master_training.csv"))
