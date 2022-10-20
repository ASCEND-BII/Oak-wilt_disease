################################################################################
### Time-Weighted Dynamic Time Warping for oak wilt
################################################################################

#-------------------------------------------------------------------------------
# Libraries

library(dtwSat)
library(data.table)
library(caret)
library(kohonen)

#-------------------------------------------------------------------------------
# Root path

path <- "/media/antonio/antonio_ssd/TRAINING"

#-------------------------------------------------------------------------------
# Read, reshape, and clean

#Read file
ts <- fread(paste0(path, "/level3_ts-pixels/master_observations.csv"))
ts <- subset(ts, date >= as.IDate("2019-01-01") &
                 date <= as.IDate("2019-12-31"))

#Reshape
ts <- dcast(ts, formula = tile + ID + Condition + x + y + metric + date ~ VI)

#Clean
ts <- ts[!is.na(ts$x)]
ts <- ts[tile != "X0017_Y0027" & ID != 25]
#Data partitions ---------------------------------------------------------------
table(ts$Condition)/length(unique(ts$date)) #n samples

#Unique samples per tile
unique_samples <- unique(ts[, 1:3])
unique_samples$id_tile <- 1:nrow(unique_samples)
unique_samples$ID <- as.numeric(unique_samples$ID)

#Split
ind_condition <- createDataPartition(unique_samples$Condition,
                                     p = 0.60,
                                     list = FALSE,
                                     times = 1)
ind_condition <- ind_condition[, 1]
train <- unique_samples[ind_condition, ]
test <- unique_samples[!ind_condition, ]

#Training and testing ----------------------------------------------------------
training <- merge(train, ts, by = c("tile", "ID", "Condition"), all.x = TRUE, all.y = FALSE)
training <- na.exclude(training)
testing <- merge(test, ts, by = c("tile", "ID", "Condition"), all.x = TRUE, all.y = FALSE)
testing <- na.exclude(testing)

#Creation of the twdtwTimeSeries object ----------------------------------------
#Function to a zoo object
zoo_object <- function(id, frame) {
  observation <- subset(frame, id_tile == id)
  object <- zoo(as.matrix(observation[, 9:12]), as.Date(observation$date))
}

#Create list
training_list <- lapply(unique(training$id_tile), zoo_object, frame = training)
testing_list <- lapply(unique(testing$id_tile), zoo_object, frame = testing)

#Create the twdtwTimeSeries
training_dtw <- new("twdtwTimeSeries", 
                   timeseries = training_list, 
                   labels = train$Condition)

testing_dtw <- new("twdtwTimeSeries", 
                    timeseries = testing_list, 
                    labels = test$Condition)

#Creation of the patterns ------------------------------------------------------

profiles_training <- createPatterns(training_dtw, 
                                   freq = 1, 
                                   formula = y ~ s(x))

profiles_testing <- createPatterns(testing_dtw, 
                                    freq = 1, 
                                    formula = y ~ s(x))

plot(profiles_training, type = "patterns")
plot(profiles_testing, type = "patterns")

weight_fun <- logisticWeight(alpha = -0.1, beta = 100)

image_path <- "/media/antonio/antonio_ssd/TRAINING/level3/test"

a  <- brick(paste0("/media/antonio/antonio_ssd/TRAINING/level3/X0014_Y0024/2016-2022_001-365_HL_TSA_SEN2L_CCI_TSI.tif"))
a <- subset(a, 70:92)
CCI  <- brick(paste0(image_path, "/", "CCI.tif"))
CCI <- subset(CCI, 70:92)
CRE <- brick(paste0(image_path, "/", "CRE.tif"))
CRE <- subset(CRE, 70:92)
KNV  <- brick(paste0(image_path, "/", "KNV.tif"))
KNV <- subset(KNV, 70:92)
NDM <- brick(paste0(image_path, "/", "NDM.tif"))
NDM <- subset(NDM, 70:92)

dates <- names(a)
dates <- paste0(substr(dates, 2, 5), "-", substr(dates, 6, 7), "-", substr(dates, 8, 9))
doy <- yday(as.Date(dates))
a <- CCI

for(i in 1:length(dates)) {
  
  a[[i]][] <- doy[i]
  
}

rts <- twdtwRaster(CCI, CRE, KNV, NDM, timeline = dates, doy = a)

library(doParallel)
library(parallel)
library(foreach)

cl <- makeCluster(28, type = "FORK")
registerDoParallel(cl)

system.time(
  twdtw_lucc <- twdtwApply(x = rts, 
                           y = profiles_training, 
                           alpha = -0.1,
                           beta = 100,
                           progress = 'text', 
                           n = 22,
                           breaks = as.Date(dates),
                           legacy = FALSE,
                           time.window = TRUE)
)


registerDoSEQ()
stopCluster(cl)




library(dtwSat)  

evi  <- brick(system.file("lucc_MT/data/evi.tif",  package = "dtwSat"))
ndvi <- brick(system.file("lucc_MT/data/ndvi.tif", package = "dtwSat"))
red  <- brick(system.file("lucc_MT/data/red.tif",  package = "dtwSat"))
blue <- brick(system.file("lucc_MT/data/blue.tif", package = "dtwSat"))
nir  <- brick(system.file("lucc_MT/data/nir.tif",  package = "dtwSat"))
mir  <- brick(system.file("lucc_MT/data/mir.tif",  package = "dtwSat"))
doy  <- brick(system.file("lucc_MT/data/doy.tif",  package = "dtwSat"))

timeline <- scan(system.file("lucc_MT/data/timeline", package = "dtwSat"), what="date")
timeline

rts <- twdtwRaster(evi, ndvi, red, blue, nir, mir, timeline = timeline, doy = doy)
rts

field_samples <- read.csv(system.file("lucc_MT/data/samples.csv", package = "dtwSat"))
head(field_samples)

library(caret)

set.seed(1) # set for reproducibility 

I <- unlist(createDataPartition(field_samples$label, p = 0.1))

training_samples <- field_samples[I,]

validation_samples <- field_samples[-I,]

training_ts <- getTimeSeries(rts, 
                             y = training_samples, 
                             proj4string = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

profiles_library <- createPatterns(training_ts, 
                                   freq = 8, 
                                   formula = y ~ s(x))

plot(profiles_library, type = "patterns")

library(doParallel)
library(parallel)
library(foreach)

cl <- makeCluster(28, type = "FORK")
registerDoParallel(cl)

system.time(
  twdtw_lucc <- twdtwApply(x = rts, 
                           y = profiles_library, 
                           alpha = -0.1,
                           beta = 50,
                           progress = 'text', 
                           minrows = 30,
                           legacy = FALSE,
                           time.window = TRUE)
)

registerDoSEQ()
stopCluster(cl)

plot(twdtw_lucc, type = "distance", time.levels = 12)

twdtw_assess <- 
  twdtwAssess(twdtw_lucc, 
              y = validation_samples, 
              proj4string = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
              conf.int = .95)

