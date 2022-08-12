################################################################################
##### 05 - Model (training - testing)
################################################################################

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(caret)
library(ggplot2)
library(viridis)
library(terra)
library(doParallel)

#-------------------------------------------------------------------------------
# Root path

path <- "/media/antonio/antonio_ssd/FORCE"
path <- "E:/FORCE"

#-------------------------------------------------------------------------------
# Reading and cleaning

#Reading -------------
X0014_Y0024 <- fread(paste0(path, "/level3_shifted/X0014_0024_VI_clean.txt"))
X0015_Y0024 <- fread(paste0(path, "/level3_shifted/X0015_0024_VI_clean.txt"))
#X0016_Y0024 <- fread(paste0(path, "/model/data/X0016_0024_dVI.txt"))

#Add tiles
X0014_Y0024$tile <- "X0014_Y0024"
X0015_Y0024$tile <- "X0015_Y0024"
#X0016_Y0024$tile <- "X0016_Y0024"

#rbind tiles
data <- rbind(X0014_Y0024, X0015_Y0024)

#Cleaning -------------

#Subset for 2019
data <- subset(data, year(as.Date(date)) == 2019)
data <- data[, c(12, 1, 2, 3, 4, 7, 8, 9, 11)]
data <- subset(data, as.Date(date) >= as.Date("2019-06-15"))
data <- subset(data, as.Date(date) <= as.Date("2019-10-01"))

#Look for area features
data <- subset(data, area >= (pi*3^2)) #radios higher than 3m

#NA exclude
#Unique combination
remove <- na.exclude(data)
unique_IDs <- remove[, .N, by= c("tile", "ID", "condition")]
unique_IDs <- unique_IDs[N == max(unique_IDs$N)]

#Final to use
data <- merge(unique_IDs, data, by = c("tile", "ID", "condition"), all.x = TRUE, all.y = FALSE)

ggplot(data) +
  geom_line(aes(x = date, y = tanh((CCI/10000)^2), colour = condition, group = interaction(tile, ID))) +
  geom_vline(xintercept = as.Date("2019-06-15")) + 
  geom_vline(xintercept = as.Date("2019-10-01"))

ggplot(data) +
  geom_line(aes(x = date, y = CCI, colour = condition, group = interaction(tile, ID))) +
  geom_vline(xintercept = as.Date("2019-06-15")) + 
  geom_vline(xintercept = as.Date("2019-10-01"))

ggplot(data) +
  geom_line(aes(x = date, y = CRE, colour = condition, group = interaction(tile, ID))) +
  geom_vline(xintercept = as.Date("2019-06-15")) + 
  geom_vline(xintercept = as.Date("2019-10-01"))

ggplot(data) +
  geom_line(aes(x = date, y = tanh((NDW/10000)^2), colour = condition, group = interaction(tile, ID))) +
  geom_vline(xintercept = as.Date("2019-06-15")) + 
  geom_vline(xintercept = as.Date("2019-10-01"))

ggplot(data) +
  geom_line(aes(x = date, y = NDW, colour = condition, group = interaction(tile, ID))) +
  geom_vline(xintercept = as.Date("2019-06-15")) + 
  geom_vline(xintercept = as.Date("2019-10-01"))


slope <- data.table()

for(i in 1:nrow(unique_IDs)) {
  
  sub_data <- subset(data, tile == unique_IDs$tile[i] & ID == unique_IDs$ID[i])
  
  trend <- lm(sub_data$CCI ~ sub_data$date)
  
  results <- data.table(tile = sub_data$tile[1],
                        ID = sub_data$ID[1],
                        condition = sub_data$condition[1],
                        slope = trend$coefficients[2],
                        intercept = trend$coefficients[1],
                        rsq = summary(trend)$r.squared,
                        maxCRE = max(sub_data$CRE),
                        minNDW = min(sub_data$NDW))
  
  slope <- rbind(slope, results)
  
}


ggplot(slope, aes(x= condition, y= slope, fill = condition)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)

ggplot(slope, aes(x= condition, y= maxCRE, fill = condition)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)

ggplot(slope, aes(x= condition, y= minNDW, fill = condition)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)
  

#Look for out layers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

slope[, slope := remove_outliers(slope), by = condition]
slope[, maxCRE := remove_outliers(maxCRE), by = condition]
slope[, minNDW := remove_outliers(minNDW), by = condition]
