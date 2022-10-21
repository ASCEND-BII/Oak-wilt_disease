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
path <- "F:/TRAINING/level3_lsf-pixels"

#-------------------------------------------------------------------------------
# NAIP 2019 observations

#Reading
X0014_Y0024 <- fread(paste0(path, "/NAIP/X0014_Y0024_lsf.txt"))
X0015_Y0024 <- fread(paste0(path, "/NAIP/X0015_Y0024_lsf.txt"))
X0016_Y0024 <- fread(paste0(path, "/NAIP/X0016_Y0024_lsf.txt"))
X0016_Y0025 <- fread(paste0(path, "/NAIP/X0016_Y0025_lsf.txt"))
X0016_Y0027 <- fread(paste0(path, "/NAIP/X0016_Y0027_lsf.txt"))
X0017_Y0026 <- fread(paste0(path, "/NAIP/X0017_Y0026_lsf.txt"))
X0017_Y0027 <- fread(paste0(path, "/NAIP/X0017_Y0027_lsf.txt"))

#Add tile
X0014_Y0024$tile <- "X0014_Y0024"
X0015_Y0024$tile <- "X0015_Y0024"
X0016_Y0024$tile <- "X0016_Y0024"
X0016_Y0025$tile <- "X0016_Y0025"
X0016_Y0027$tile <- "X0016_Y0027"
X0017_Y0026$tile <- "X0017_Y0026"
X0017_Y0027$tile <- "X0017_Y0027"

#Rbind tiles
NAIP_data <- rbind(X0014_Y0024, X0015_Y0024, X0016_Y0024, 
              X0016_Y0025, X0016_Y0027, X0017_Y0026, 
              X0017_Y0027)

#Add reference sensor
NAIP_data$sensor <- "NAIP"

#Subset year
NAIP_data <- subset(NAIP_data, year == "2019")

#Dcast
NAIP_data <- dcast(NAIP_data, tile + sensor + ID + x + y + Condition + year + VI ~ metric)

#-------------------------------------------------------------------------------
# AVIRIS-NG 2018 observations

#Reading
X0014_Y0024 <- fread(paste0(path, "/AVIRIS/X0014_Y0024_AVIRIS_lsf.txt"))
X0015_Y0024 <- fread(paste0(path, "/AVIRIS/X0015_Y0024_AVIRIS_lsf.txt"))
X0016_Y0024 <- fread(paste0(path, "/AVIRIS/X0016_Y0024_AVIRIS_lsf.txt"))
X0017_Y0024 <- fread(paste0(path, "/AVIRIS/X0017_Y0024_AVIRIS_lsf.txt"))

#Add tile
X0014_Y0024$tile <- "X0014_Y0024"
X0015_Y0024$tile <- "X0015_Y0024"
X0016_Y0024$tile <- "X0016_Y0024"
X0017_Y0024$tile <- "X0017_Y0024"

#Rbind tiles
AVIRIS_data <- rbind(X0014_Y0024, X0015_Y0024, X0016_Y0024, X0017_Y0024)

#Add reference sensor
AVIRIS_data$sensor <- "AVIRIS-NG"

#Subset year
AVIRIS_data <- subset(AVIRIS_data, year == "2018")

#Dcast
AVIRIS_data <- dcast(AVIRIS_data, tile + sensor + ID + x + y + Condition + year + VI ~ metric)

#-------------------------------------------------------------------------------
# Match observations for training
#-------------------------------------------------------------------------------

#Test name of variables
all.equal(colnames(AVIRIS_data), colnames(NAIP_data))

#Rbind sensors
data <- rbind(NAIP_data, AVIRIS_data)

#Test for CCI
data <- subset(data, VI == "CCI")
data$Condition <- as.factor(data$Condition)
data$Condition <- factor(data$Condition, levels = c("healthy", "wilted", "dead"))

ggplot(data[VPA > 0], aes(Condition, VPA/VGM, fill = sensor, group = interaction(sensor, Condition))) + 
  geom_jitter(size=0.4, alpha=0.9) +
  geom_boxplot() +
  ylab('density') +
  theme_minimal() +
  theme(axis.title.x = element_blank())

ggplot(data[VPA > 0], aes(Condition, VGV, fill = sensor, group = interaction(sensor, Condition))) + 
  geom_jitter(size=0.4, alpha=0.9) +
  geom_boxplot() +
  ylab('LSF') +
  theme_minimal() +
  theme(axis.title.x = element_blank())

ggplot(data[VPA > 0], aes(Condition, (IBT-IST)/IBT, fill = sensor, group = interaction(sensor, Condition))) + 
  geom_jitter(size=0.4, alpha=0.9) +
  geom_boxplot() +
  ylab('LSF') +
  theme_minimal() +
  theme(axis.title.x = element_blank())

ggplot(data[VPA > 0], aes(Condition, VPS, fill = sensor, group = interaction(sensor, Condition))) + 
  geom_jitter(size=0.4, alpha=0.9) +
  geom_boxplot() +
  ylab('LSF') +
  theme_minimal() +
  theme(axis.title.x = element_blank())

