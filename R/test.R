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
data <- data[, c(12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 10)]
data <- subset(data, as.Date(date) >= as.Date("2019-06-01"))
data <- subset(data, as.Date(date) <= as.Date("2019-09-04"))

#Look for area features
data <- subset(data, area >= (pi*3^2)) #radios higher than 3m

#NA exclude
#Unique combination
remove <- na.exclude(data)
unique_IDs <- remove[, .N, by= c("tile", "ID", "condition", "x", "y")]
unique_IDs <- unique_IDs[N == max(unique_IDs$N)]
unique_IDs$N <- 1:nrow(unique_IDs)

#Final to use
data <- merge(unique_IDs, data, by = c("tile", "ID", "condition", "x", "y"), all.x = TRUE, all.y = FALSE)

ggplot(data) +
  geom_line(aes(x = date, y = CCI/10000, colour = condition, group = interaction(N))) +
  geom_vline(xintercept = as.Date("2019-06-15")) + 
  geom_vline(xintercept = as.Date("2019-09-04"))

ggplot(data) +
  geom_line(aes(x = date, y = CRE/1000, colour = condition, group = interaction(N))) +
  geom_vline(xintercept = as.Date("2019-06-15")) + 
  geom_vline(xintercept = as.Date("2019-09-04"))

ggplot(data) +
  geom_line(aes(x = date, y = NDW/10000, colour = condition, group = interaction(N))) +
  geom_vline(xintercept = as.Date("2019-06-15")) + 
  geom_vline(xintercept = as.Date("2019-09-04"))

ggplot(data) +
  geom_line(aes(x = date, y = KNV/10000, colour = condition, group = interaction(N))) +
  geom_vline(xintercept = as.Date("2019-06-15")) + 
  geom_vline(xintercept = as.Date("2019-09-04"))

slope <- data.table()

n_pixel <- unique(data$N)

for(j in 1:length(n_pixel)) {
  
  sub_data <- subset(data, N == n_pixel[j])
  
  trend <- lm(sub_data$CCI ~ sub_data$date)
  
  results <- data.table(tile = sub_data$tile[1],
                        ID = sub_data$ID[1],
                        N = n_pixel[j],
                        condition = sub_data$condition[1],
                        area = sub_data$area[1],
                        weight = sub_data$weight[1],
                        slope = trend$coefficients[2],
                        intercept = trend$coefficients[1],
                        rsq = summary(trend)$r.squared,
                        maxCRE = sub_data$CRE[4],
                        minNDW = tanh((sub_data$NDW[4]/10000)^2)*10000,
                        init_KDW = sub_data$KNV[1],
                        mean_KDW = mean(sub_data$KNV))
  
  slope <- rbind(slope, results)
  
}

slope <- subset(slope, mean_KDW >= 5000)
slope <- subset(slope, minNDW >= 5000)

ggplot(slope, aes(x= condition, y= slope, fill = condition)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)

ggplot(slope, aes(x= condition, y= intercept, fill = condition)) +
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

ggplot(slope, aes(x= condition, y= init_KDW, fill = condition)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)

ggplot(slope, aes(x= condition, y= mean_KDW, fill = condition)) +
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
slope[, init_KDW := remove_outliers(init_KDW), by = condition]

slope <- na.exclude(slope)

#Export for training
fwrite(slope, paste0(path, "/model/data/data_clean.csv"))

#-------------------------------------------------------------------------------
# Model preparation

#Read
data <- fread(paste0(path, "/model/data/data_clean.csv"))
data <- subset(data, weight >= 0.5)

#Data split
data[condition == "wilted", factor_wilted := "wilted", ]
data[condition != "wilted", factor_wilted := "non_wilted", ]

data[condition == "healthy", factor_healthy := "healthy", ]
data[condition != "healthy", factor_healthy := "non_healthy", ]

data[condition == "dead", factor_dead := "dead", ]
data[condition != "dead", factor_dead := "non_dead", ]

#Data partitions ---------------------------------------------------------------
#Wilted ---------------------------
table(data$factor_wilted) #625 529 

#Separate
wilted <- subset(data, factor_wilted == "wilted")
no_wilted <- subset(data, factor_wilted == "non_wilted")

#Split
ind_wilted <- createDataPartition(wilted$tile,
                                  p = 0.75,
                                  list = FALSE,
                                  times = 1)

ind_no_wilted <- createDataPartition(no_wilted$tile,
                                     p = 0.75,
                                     list = FALSE,
                                     times = 1)


#Healthy ---------------------------
table(data$factor_healthy) #347 807

#Separate
healthy <- subset(data, factor_healthy == "healthy")
no_healthy <- subset(data, factor_healthy == "non_healthy")

#Split
ind_healthy <- createDataPartition(healthy$tile,
                                   p = 0.75,
                                   list = FALSE,
                                   times = 1)

ind_no_healthy <- createDataPartition(no_healthy$tile,
                                      p = 0.75,
                                      list = FALSE,
                                      times = 1)

#Dead ---------------------------
table(data$factor_dead) #278 876

#Separate
dead <- subset(data, factor_dead == "dead")
no_dead <- subset(data, factor_dead == "non_dead")

ind_dead <- createDataPartition(dead$tile,
                                p = 0.75,
                                list = FALSE,
                                times = 1)

ind_no_dead <- createDataPartition(no_dead$tile,
                                   p = 0.75,
                                   list = FALSE,
                                   times = 1)

#Get training and testing export -----------------------------------------------
#wilted
wilted_training <- rbind(wilted[ind_wilted], no_wilted[ind_no_wilted])
wilted_testing <- rbind(wilted[!ind_wilted], no_wilted[!ind_no_wilted])

fwrite(wilted_training, paste0(path, "/model/training/wilted_training.csv"))
fwrite(wilted_testing, paste0(path, "/model/training/wilted_testing.csv"))

#Healthy
healthy_training <- rbind(healthy[ind_healthy], no_healthy[ind_no_healthy])
healthy_testing <- rbind(healthy[!ind_healthy], no_healthy[!ind_no_healthy])

fwrite(healthy_training, paste0(path, "/model/training/healthy_training.csv"))
fwrite(healthy_testing, paste0(path, "/model/training/healthy_testing.csv"))

#Dead
dead_training <- rbind(dead[ind_dead], no_dead[ind_no_dead])
dead_testing <- rbind(dead[!ind_dead], no_dead[!ind_no_dead])

fwrite(dead_training, paste0(path, "/model/training/dead_training.csv"))
fwrite(dead_testing, paste0(path, "/model/training/dead_testing.csv"))

#-------------------------------------------------------------------------------
# Model Training

#Repeated 10-fold cross-validation
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           savePredictions = TRUE)

wilted_names <- c("factor_wilted", "slope", "maxCRE", "init_KDW", "mean_KDW")
healthy_names <- c("factor_healthy", "slope", "maxCRE", "init_KDW", "mean_KDW")
dead_names <- c("factor_dead", "slope", "maxCRE", "init_KDW", "mean_KDW")

#Select columns of interest
wilted_weights <- wilted_training$weight
wilted_training <- wilted_training[, ..wilted_names]
healthy_weights <- healthy_training$weight
healthy_training <- healthy_training[, ..healthy_names]
dead_weights <- dead_training$weight
dead_training <- dead_training[, ..dead_names]

#Model
model_training <- function(wilted_training, healthy_training, dead_training) {
  
  set.seed(825)
  wilted_model <- train(factor_wilted ~ ., data = wilted_training, 
                        method = 'bayesglm', 
                        trControl = fitControl,
                        weights = wilted_weights*100,
                        metric = "ROC")
  
  set.seed(825)
  healthy_model <- train(factor_healthy ~ ., data = healthy_training, 
                         method = 'bayesglm', 
                         trControl = fitControl,
                         weights = healthy_weights*100,
                         metric = "ROC")
  
  set.seed(825)
  dead_model <- train(factor_dead ~ ., data = dead_training, 
                      method = 'bayesglm', 
                      trControl = fitControl,
                      weights = dead_weights*100,
                      metric = "ROC")
  
  set.seed(825)
  dead_model <- train(factor_dead ~ ., data = dead_training, 
                      method = 'bayesglm', 
                      trControl = fitControl,
                      weights = dead_weights*100,
                      metric = "ROC")
  
  return(list(wilted = wilted_model,
              healthy = healthy_model,
              dead = dead_model))
  
}

models <- model_training(wilted_training, healthy_training, dead_training)

#Export model
saveRDS(models, "data/models/models.rds")

#-------------------------------------------------------------------------------
# Model Testing

#Select columns of interest
wilted_testing <- wilted_testing[, ..wilted_names]
healthy_testing <- healthy_testing[, ..healthy_names]
dead_testing <- dead_testing[, ..dead_names]

#Model testing

model_testing <- function(models, 
                          wilted_training, wilted_testing, 
                          healthy_training, healthy_testing, 
                          dead_training, dead_testing) {
  
  #Wilted
  wilted_training_m <- data.frame(observed = as.factor(wilted_training$factor_wilted),
                                  predicted = predict(models$wilted))
  
  wilted_testing_m <- data.frame(observed = as.factor(wilted_testing$factor_wilted),
                                 predicted = predict(models$wilted, newdata = wilted_testing))
  
  wilted_training_cm <- caret::confusionMatrix(wilted_training_m$observed, wilted_training_m$predicted)
  wilted_testing_cm <- caret::confusionMatrix(wilted_testing_m$observed, wilted_testing_m$predicted)
  
  #Healthy
  healthy_training_m <- data.frame(observed = as.factor(healthy_training$factor_healthy),
                                   predicted = predict(models$healthy))
  
  healthy_testing_m <- data.frame(observed = as.factor(healthy_testing$factor_healthy),
                                  predicted = predict(models$healthy, newdata = healthy_testing))
  
  healthy_training_cm <- caret::confusionMatrix(healthy_training_m$observed, healthy_training_m$predicted)
  healthy_testing_cm <- caret::confusionMatrix(healthy_testing_m$observed, healthy_testing_m$predicted)
  
  #Dead
  dead_training_m <- data.frame(observed = as.factor(dead_training$factor_dead),
                                predicted = predict(models$dead))
  
  dead_testing_m <- data.frame(observed = as.factor(dead_testing$factor_dead),
                               predicted = predict(models$dead, newdata = dead_testing))
  
  dead_training_cm <- caret::confusionMatrix(dead_training_m$observed, dead_training_m$predicted)
  dead_testing_cm <- caret::confusionMatrix(dead_testing_m$observed, dead_testing_m$predicted)
  
  #Merge
  wilted_training <- cbind(data.table(Condition = "Wilted", Step = "Training"), 
                           matrix(wilted_training_cm$overall, nrow = 1), 
                           matrix(wilted_training_cm$byClass, nrow = 1))
  
  wilted_testing <- cbind(data.table(Condition = "Wilted", Step = "Testing"), 
                          matrix(wilted_testing_cm$overall, nrow = 1), 
                          matrix(wilted_testing_cm$byClass, nrow = 1))
  
  healthy_training <- cbind(data.table(Condition = "Healthy", Step = "Training"), 
                            matrix(healthy_training_cm$overall, nrow = 1), 
                            matrix(healthy_training_cm$byClass, nrow = 1))
  
  healthy_testing <- cbind(data.table(Condition = "Healthy", Step = "Testing"), 
                           matrix(healthy_testing_cm$overall, nrow = 1), 
                           matrix(healthy_testing_cm$byClass, nrow = 1))
  
  dead_training <- cbind(data.table(Condition = "Dead", Step = "Training"), 
                         matrix(dead_training_cm$overall, nrow = 1), 
                         matrix(dead_training_cm$byClass, nrow = 1))
  
  dead_testing <- cbind(data.table(Condition = "Dead", Step = "Testing"), 
                        matrix(dead_testing_cm$overall, nrow = 1), 
                        matrix(dead_testing_cm$byClass, nrow = 1))
  
  frame <- rbind(wilted_training, wilted_testing,
                 healthy_training, healthy_testing,
                 dead_training, dead_testing)
  
  colnames(frame)[3:9] <- names(dead_training_cm$overall)
  colnames(frame)[10:20] <- names(dead_training_cm$byClass)
  
  return(frame)
  
}

validation <- model_testing(models, 
                            wilted_training, wilted_testing, 
                            healthy_training, healthy_testing, 
                            dead_training, dead_testing)











