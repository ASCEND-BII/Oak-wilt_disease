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

#-------------------------------------------------------------------------------
# Root path

path <- "/media/antonio/antonio_ssd/FORCE/corregistration/application/level3_shifted_phenology"
path <- "E:/FORCE/corregistration/application/level3_shifted_phenology"

#-------------------------------------------------------------------------------
# Reading and cleaning

#Reading -------------
X0014_Y0024 <- fread(paste0(path, "/X0014_0024_phenology_clean.txt"))
X0015_Y0024 <- fread(paste0(path, "/X0015_0024_phenology_clean.txt"))
#X0016_Y0024 <- fread(paste0(path, "/model/data/X0016_0024_dVI.txt"))

#rbind tiles
data <- rbind(X0014_Y0024, X0015_Y0024)

#Look for area and year features
data <- subset(data, area >= (pi*3^2)) #radios higher than 3m
data <- data[year == "2019"]

#NA exclude
#Unique combination
remove <- na.exclude(data)
unique_IDs <- remove[, .N, by= c("tile", "ID", "condition")]
unique_IDs$N <- 1:nrow(unique_IDs)

#Final to use
data <- merge(unique_IDs, data, by = c("tile", "ID", "condition"), all.x = TRUE, all.y = FALSE)
data$row <- 1:nrow(data)

#-------------------------------------------------------------------------------
# Model preparation

#Read
#data <- fread(paste0(path, "/model/data/data_clean.csv"))
#data <- subset(data, weight >= 0.5)

#Data split
data[condition == "wilted", factor_wilted := "wilted", ]
data[condition != "wilted", factor_wilted := "non_wilted", ]

data[condition == "healthy", factor_healthy := "healthy", ]
data[condition != "healthy", factor_healthy := "non_healthy", ]

data[condition == "dead", factor_dead := "dead", ]
data[condition != "dead", factor_dead := "non_dead", ]

data <- data[VI == "CCI"]

#Get pixel with the high weight
frame <- data[, row[which.max(weight)], by = c("N", "VI")]
colnames(frame)[3] <- "row"
frame <- na.exclude(frame)

#Select rows
data <- merge(frame, data, by = c("N", "VI", "row"), all.x = TRUE, all.y = FALSE)
data <- data[weight >= 0.5]

metrics <- c("tile", "condition", "factor_wilted", "factor_healthy", "factor_dead", "weight", "area", "VGV", "VGA", "VPA", "RMF")

data <- data[, ..metrics]

#Look for out layers
#remove_outliers <- function(x, na.rm = TRUE, ...) {
#  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
#  H <- 1.5 * IQR(x, na.rm = na.rm)
#  y <- x
#  y[x < (qnt[1] - H)] <- NA
#  y[x > (qnt[2] + H)] <- NA
#  y
#}

#data[, VGV := remove_outliers(VGV), by = condition]
#data[, VGA := remove_outliers(VGA), by = condition]
#data[, VPA := remove_outliers(VPA), by = condition]
#data[, RMF := remove_outliers(RMF), by = condition]

#data <- na.exclude(data)

#Data partitions ---------------------------------------------------------------
#Condition ---------------------------

data <- fread("master.csv")
data <- data[VI == "CCI"]

table(data$condition) #625 529 

#Split
ind_condition <- createDataPartition(data$condition,
                                     p = 0.75,
                                     list = FALSE,
                                     times = 1)

#Wilted ---------------------------
table(data$factor_wilted) #625 529 

#Separate
wilted <- subset(data, factor_wilted == "wilted")
no_wilted <- subset(data, factor_wilted == "non_wilted")

#Split
ind_wilted <- createDataPartition(wilted$tile,
                                  p = 0.6,
                                  list = FALSE,
                                  times = 1)

ind_no_wilted <- createDataPartition(no_wilted$tile,
                                     p = 0.6,
                                     list = FALSE,
                                     times = 1)


#Healthy ---------------------------
table(data$factor_healthy) #347 807

#Separate
healthy <- subset(data, factor_healthy == "healthy")
no_healthy <- subset(data, factor_healthy == "non_healthy")

#Split
ind_healthy <- createDataPartition(healthy$tile,
                                   p = 0.6,
                                   list = FALSE,
                                   times = 1)

ind_no_healthy <- createDataPartition(no_healthy$tile,
                                      p = 0.6,
                                      list = FALSE,
                                      times = 1)

#Dead ---------------------------
table(data$factor_dead) #278 876

#Separate
dead <- subset(data, factor_dead == "dead")
no_dead <- subset(data, factor_dead == "non_dead")

ind_dead <- createDataPartition(dead$tile,
                                p = 0.6,
                                list = FALSE,
                                times = 1)

ind_no_dead <- createDataPartition(no_dead$tile,
                                   p = 0.6,
                                   list = FALSE,
                                   times = 1)

#Get training and testing export -----------------------------------------------
#Condition
condition_training <- data[ind_condition]
condition_testing <- data[!ind_condition]

#fwrite(condition_training, paste0(path, "/model/training/condition_training.csv"))
#fwrite(condition_testing, paste0(path, "/model/training/condition_testing.csv"))

#wilted
wilted_training <- rbind(wilted[ind_wilted], no_wilted[ind_no_wilted])
wilted_testing <- rbind(wilted[!ind_wilted], no_wilted[!ind_no_wilted])

#fwrite(wilted_training, paste0(path, "/model/training/wilted_training.csv"))
#fwrite(wilted_testing, paste0(path, "/model/training/wilted_testing.csv"))

#Healthy
healthy_training <- rbind(healthy[ind_healthy], no_healthy[ind_no_healthy])
healthy_testing <- rbind(healthy[!ind_healthy], no_healthy[!ind_no_healthy])

#fwrite(healthy_training, paste0(path, "/model/training/healthy_training.csv"))
#fwrite(healthy_testing, paste0(path, "/model/training/healthy_testing.csv"))

#Dead
dead_training <- rbind(dead[ind_dead], no_dead[ind_no_dead])
dead_testing <- rbind(dead[!ind_dead], no_dead[!ind_no_dead])

#fwrite(dead_training, paste0(path, "/model/training/dead_training.csv"))
#fwrite(dead_testing, paste0(path, "/model/training/dead_testing.csv"))

#-------------------------------------------------------------------------------
# Model Training
#metrics <- c("VGV", "VGA", "VPA", "RMF")

metrics <- c("VGV", "VSS", "VPA", "IGS")

condition_names <-  c("condition", metrics)
wilted_names <-  c("factor_wilted", metrics)
healthy_names <- c("factor_healthy", metrics)
dead_names <- c("factor_dead", metrics)

#Select columns of interest
condition_weights <- condition_training$weight
wilted_weights <- wilted_training$weight
healthy_weights <- healthy_training$weight
dead_weights <- dead_training$weight

condition_training <- condition_training[, ..condition_names]
wilted_training <- wilted_training[, ..wilted_names]
healthy_training <- healthy_training[, ..healthy_names]
dead_training <- dead_training[, ..dead_names]

#Model
model_training <- function(condition_training, wilted_training, healthy_training, dead_training) {
  
  #Repeated 10-fold cross-validation
  fitControl <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 10,
                             classProbs = TRUE,
                             summaryFunction = twoClassSummary,
                             savePredictions = TRUE)
  
  #Repeated 10-fold cross-validation
  #cfitControl <- trainControl(method = "repeatedcv",
  #                            number = 10,
  #                            repeats = 10,
  #                            classProbs = TRUE,
  #                            savePredictions = TRUE)
  
  #set.seed(825)
  #condition_model <- train(condition ~ ., data = condition_training, 
  #                         method = 'bayesglm', 
  #                         trControl = cfitControl)
  #weights = condition_weights*100)
  
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
  
  return(list(wilted = wilted_model,
              healthy = healthy_model,
              dead = dead_model))
  
}

models <- model_training(condition_training, wilted_training, healthy_training, dead_training)

#Export model
saveRDS(models, "data/models/models.rds")

#-------------------------------------------------------------------------------
# Model Testing

#Select columns of interest
condition_testing <- condition_testing[, ..condition_names]
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

validation


wilted <- models$wilted$finalModel$coefficients
healthy <- models$healthy$finalModel$coefficients
dead <- models$dead$finalModel$coefficients

wilted <- round(wilted, 4)
healthy <- round(healthy, 4)
dead <- round(dead, 4)

wilted <- wilted[order(wilted)]
healthy <- healthy[order(healthy)]
dead <- dead[order(dead)]
