################################################################################
##### 04 - Data cleaning SOM
################################################################################

#' @description It takes the time series observations to define if a extracted 
#' pixel is a noise observation or not.

#-------------------------------------------------------------------------------
#' Libraries

library(data.table)
library(kohonen)
library(dplyr)
library(sits)

#-------------------------------------------------------------------------------
#' Root path

path <- "/media/antonio/antonio_ssd/TRAINING/level3_ts-pixels"

#-------------------------------------------------------------------------------
#' Reading and cleaning

#Reading
X0014_Y0024 <- fread(paste0(path, "/X0014_Y0024_ts.txt"))
X0015_Y0024 <- fread(paste0(path, "/X0015_Y0024_ts.txt"))
X0016_Y0024 <- fread(paste0(path, "/X0016_Y0024_ts.txt"))
X0016_Y0025 <- fread(paste0(path, "/X0016_Y0025_ts.txt"))
X0016_Y0027 <- fread(paste0(path, "/X0016_Y0027_ts.txt"))
X0017_Y0026 <- fread(paste0(path, "/X0017_Y0026_ts.txt"))
X0017_Y0027 <- fread(paste0(path, "/X0017_Y0027_ts.txt"))

#Add tile
X0014_Y0024$tile <- "X0014_Y0024"
X0015_Y0024$tile <- "X0015_Y0024"
X0016_Y0024$tile <- "X0016_Y0024"
X0016_Y0025$tile <- "X0016_Y0025"
X0016_Y0027$tile <- "X0016_Y0027"
X0017_Y0026$tile <- "X0017_Y0026"
X0017_Y0027$tile <- "X0017_Y0027"

#rbind tiles
data <- rbind(X0014_Y0024, X0015_Y0024, X0016_Y0024, 
              X0016_Y0025, X0016_Y0027, X0017_Y0026, 
              X0017_Y0027)

#Year of interest
data <- subset(data, date >= as.IDate("2019-01-01") &
                     date <= as.IDate("2019-12-31"))
data <- data[metric == "TSI"]
data <- dcast(data, tile + ID + Condition + x + y + metric + date ~ VI)
data <- na.exclude(data)

#Sits
for_sits <- unique(data[ , c("x", "y", "Condition")])
for_sits$start_date <- as.Date(min(data$date))
for_sits$end_date <- as.Date(max(data$date))
for_sits$cube <- "FORCE"
colnames(for_sits) <- c("longitude", "latitude", "label", "start_date", "end_date", "cube")
for_sits <- for_sits[, c("longitude", "latitude", "start_date", "end_date", "label", "cube")]
for_sits$label <- as.factor(for_sits$label)
for_sits$label <- factor(for_sits$label, levels = c("healthy", "wilted", "dead"))
sits <- as_tibble(for_sits)
sits$time_series <- NA
sits$remove <- NA

#Add the time series
for(i in 1:nrow(for_sits)) {
  
  ts <- subset(data, x == for_sits$longitude[i] &
                     y == for_sits$latitude[i])
  ts <- ts[, c(7:11)]
  colnames(ts)[1] <- "Index"
  ts <- as_tibble(ts)
  ts <- na.exclude(ts)
  
  if(nrow(ts) == 23) {
    sits$time_series[i] <- list(ts)
    sits$remove[i] <- 0
  } else {
    sits$remove[i] <- 1
  }
}

#Remove incomplete time series
sits <- subset(sits, remove == 0)
sits <- sits[, c(1:7)]

#Create the SOM ----------------------------------------------------------------

#SOM 0.5
som_map <- sits_som_map(sits, 
                        grid_xdim = 12, 
                        grid_ydim = 12, 
                        alpha = 0.5,
                        rlen = 100,
                        distance = "euclidean",
                        som_radius = 2)
#SOM map
plot(som_map, band = 1)

#Create the clusters
clusters_som <- sits_som_evaluate_cluster(som_map)

#Cluster evaluation
plot(clusters_som)

#Add labels
new_samples_5 <- sits_som_clean_samples(som_map,
                                        prior_threshold = 0.5,
                                        posterior_threshold = 0.5,
                                        keep = c("clean", "analyze", "remove"))

table(new_samples_5$eval)

#SOM 0.4
som_map <- sits_som_map(sits, 
                        grid_xdim = 12, 
                        grid_ydim = 12, 
                        alpha = 0.4,
                        rlen = 100,
                        distance = "euclidean",
                        som_radius = 2)
#SOM map
plot(som_map, band = 1)

#Create the clusters
clusters_som <- sits_som_evaluate_cluster(som_map)

#Cluster evaluation
plot(clusters_som)

#Add labels
new_samples_4 <- sits_som_clean_samples(som_map,
                                        prior_threshold = 0.5,
                                        posterior_threshold = 0.5,
                                        keep = c("clean", "analyze", "remove"))

table(new_samples_4$eval)


#SOM 0.3
som_map <- sits_som_map(sits, 
                        grid_xdim = 12, 
                        grid_ydim = 12, 
                        alpha = 0.3,
                        rlen = 100,
                        distance = "euclidean",
                        som_radius = 2)
#SOM map
plot(som_map, band = 1)

#Create the clusters
clusters_som <- sits_som_evaluate_cluster(som_map)

#Cluster evaluation
plot(clusters_som)

#Add labels
new_samples_3 <- sits_som_clean_samples(som_map,
                                        prior_threshold = 0.5,
                                        posterior_threshold = 0.5,
                                        keep = c("clean", "analyze", "remove"))

table(new_samples_3$eval)

#SOM 0.2
som_map <- sits_som_map(sits, 
                        grid_xdim = 12, 
                        grid_ydim = 12, 
                        alpha = 0.2,
                        rlen = 100,
                        distance = "euclidean",
                        som_radius = 2)
#SOM map
plot(som_map, band = 1)

#Create the clusters
clusters_som <- sits_som_evaluate_cluster(som_map)

#Cluster evaluation
plot(clusters_som)

#Add labels
new_samples_3 <- sits_som_clean_samples(som_map,
                                        prior_threshold = 0.5,
                                        posterior_threshold = 0.5,
                                        keep = c("clean", "analyze", "remove"))

table(new_samples_3$eval)

#SOM 0.2
som_map <- sits_som_map(sits, 
                        grid_xdim = 12, 
                        grid_ydim = 12, 
                        alpha = 0.2,
                        rlen = 100,
                        distance = "euclidean",
                        som_radius = 2)
#SOM map
plot(som_map, band = 1)

#Create the clusters
clusters_som <- sits_som_evaluate_cluster(som_map)

#Cluster evaluation
plot(clusters_som)

#Add labels
new_samples_2 <- sits_som_clean_samples(som_map,
                                        prior_threshold = 0.5,
                                        posterior_threshold = 0.5,
                                        keep = c("clean", "analyze", "remove"))

table(new_samples_2$eval)

#SOM 0.1
som_map <- sits_som_map(sits, 
                        grid_xdim = 12, 
                        grid_ydim = 12, 
                        alpha = 0.1,
                        rlen = 100,
                        distance = "euclidean",
                        som_radius = 2)
#SOM map
plot(som_map, band = 1)

#Create the clusters
clusters_som <- sits_som_evaluate_cluster(som_map)

#Cluster evaluation
plot(clusters_som)

#Add labels
new_samples_1 <- sits_som_clean_samples(som_map,
                                        prior_threshold = 0.5,
                                        posterior_threshold = 0.5,
                                        keep = c("clean", "analyze", "remove"))

table(new_samples_1$eval)

#-------------------------------------------------------------------------------
#' Define a noise observation

new_samples <- new_samples_1[, c(1, 2, 5, 11)]
new_samples$eva_2 <- new_samples_2[, c(11)]
new_samples$eva_3 <- new_samples_3[, c(11)]
new_samples$eva_4 <- new_samples_4[, c(11)]
new_samples$eva_5 <- new_samples_5[, c(11)]
 
new_samples <- as.data.table(new_samples)
colnames(new_samples) <- c("x", "y", "Condition", "eval1", "eval2", "eval3", "eval4", "eval5")

new_samples[, remove := all(c(eval1, eval2, eval3, eval4, eval5) == "remove"), by = seq_along(1:nrow(new_samples))]

clean_samples <- new_samples[remove == FALSE]

fwrite(clean_samples[, 1:3], paste0(path, "/clean_samples.csv"))

