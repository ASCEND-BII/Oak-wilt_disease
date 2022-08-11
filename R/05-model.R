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

#-------------------------------------------------------------------------------
# Reading and cleaning

#Reading -------------

X0014_Y0024 <- fread(paste0(path, "/model/data/X0014_0024_dkVI.txt"))
X0015_Y0024 <- fread(paste0(path, "/model/data/X0015_0024_dkVI.txt"))
#X0016_Y0024 <- fread(paste0(path, "/model/data/X0016_0024_dVI.txt"))

#Add tiles
X0014_Y0024$tile <- "X0014_Y0024"
X0015_Y0024$tile <- "X0015_Y0024"
#X0016_Y0024$tile <- "X0016_Y0024"

#rbind tiles
data <- rbind(X0014_Y0024, X0015_Y0024)

#Cleaning -------------

#Subset for 2019
data <- subset(data, date == "2019")

#Look for area features
data <- subset(data, area >= (pi*3^2)) #radios higher than 3m

#NA exclude
data <- na.exclude(data)

#Look for out layers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

data[, dCCI := remove_outliers(dCCI), by = condition]
data[, dNDW := remove_outliers(dNDW), by = condition]
data[, dCRE := remove_outliers(dCRE), by = condition]
data[, kND := remove_outliers(kND), by = condition]
data <- na.exclude(data)

#dCCI
ggplot(data, aes(x= condition, y= dCCI, fill = condition)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)

#dNDW
ggplot(data, aes(x= condition, y= dNDW, fill = condition)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)

#dCRE
ggplot(data, aes(x= condition, y= dCRE, fill = condition)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)

#dkND
ggplot(data, aes(x= condition, y= kND, fill = condition)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)

#Data should be clean and it is ready to export

#Export for training
fwrite(data, paste0(path, "/model/data/data_clean.csv"))

#-------------------------------------------------------------------------------
# Model preparation

#Read
data <- fread(paste0(path, "/model/data/data_clean.csv"))

#Data split
data[condition == "wilted", factor_wilted := "wilted", ]
data[condition != "wilted", factor_wilted := "non_wilted", ]

data[condition == "healthy", factor_healthy := "healthy", ]
data[condition != "healthy", factor_healthy := "non_healthy", ]

data[condition == "dead", factor_dead := "dead", ]
data[condition != "dead", factor_dead := "non_dead", ]

#Data partitions ---------------------------------------------------------------
#Wilted ---------------------------
table(data$factor_wilted) #629 508 

#Separate
wilted <- subset(data, factor_wilted == "wilted")
no_wilted <- subset(data, factor_wilted == "non_wilted")

#Split
ind_wilted <- createDataPartition(wilted$tile,
                                  p = 0.5997994,
                                  list = FALSE,
                                  times = 1)

ind_no_wilted <- createDataPartition(no_wilted$tile,
                                     p = 0.5507726,
                                     list = FALSE,
                                     times = 1)


#Healthy ---------------------------
table(data$factor_healthy) #358 779

#Separate
healthy <- subset(data, factor_healthy == "healthy")
no_healthy <- subset(data, factor_healthy == "non_healthy")

#Split
ind_healthy <- createDataPartition(healthy$tile,
                                  p = 0.6,
                                  list = FALSE,
                                  times = 1)

ind_no_healthy <- createDataPartition(no_healthy$tile,
                                     p = 0.39,
                                     list = FALSE,
                                     times = 1)

#Dead ---------------------------
table(data$factor_dead) #271 866

#Separate
dead <- subset(data, factor_dead == "dead")
no_dead <- subset(data, factor_dead == "non_dead")

ind_dead <- createDataPartition(dead$tile,
                                  p = 0.6004289,
                                  list = FALSE,
                                  times = 1)

ind_no_dead <- createDataPartition(no_dead$tile,
                                     p = 0.1990993,
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

fwrite(training, paste0(path, "/model/training/healthy_training.csv"))
fwrite(testing, paste0(path, "/model/training/healthy_testing.csv"))

#Dead
dead_training <- rbind(dead[ind_dead], no_dead[ind_no_dead])
dead_testing <- rbind(dead[!ind_dead], no_dead[!ind_no_dead])

fwrite(training, paste0(path, "/model/training/dead_training.csv"))
fwrite(testing, paste0(path, "/model/training/dead_testing.csv"))

#-------------------------------------------------------------------------------
# Model Training

#Repeated 10-fold cross-validation
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           savePredictions = TRUE)

#Select columns of interest
wilted_weights <- wilted_training$weight
wilted_training <- wilted_training[, c(13, 8:11)]
healthy_weights <- healthy_training$weight
healthy_training <- healthy_training[, c(14, 8:11)]
dead_weights <- dead_training$weight
dead_training <- dead_training[, c(15, 8:11)]


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
wilted_testing <- wilted_testing[, c(13, 8:11)]
healthy_testing <- healthy_testing[, c(14, 8:11)]
dead_testing <- dead_testing[, c(15, 8:11)]

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














prob <- predict(models$wilted, type = "prob")[, 2]
plot(prob ~ wilted_training$dCCI)
plot(prob ~ wilted_training$dNDW)
plot(prob ~ wilted_training$dCRE)
plot(prob ~ wilted_training$kND)

prob <- predict(models$healthy, type = "prob")[, 2]
plot(prob ~ healthy_training$dCCI)
plot(prob ~ healthy_training$dNDW)
plot(prob ~ healthy_training$dCRE)
plot(prob ~ healthy_training$kND)

prob <- predict(models$dead, type = "prob")[, 2]
plot(prob ~ dead_training$dCCI)
plot(prob ~ dead_training$dNDW)
plot(prob ~ dead_training$dCRE)
plot(prob ~ dead_training$kND)


#Get coefficients
intercept <- summary(bayesglm_model$finalModel)$coeff[1, 1]
coeff_dCCI <- summary(bayesglm_model$finalModel)$coeff[2, 1]
coeff_dNDW <- summary(bayesglm_model$finalModel)$coeff[3, 1]
coeff_dCRE <- summary(bayesglm_model$finalModel)$coeff[4, 1]

numerador <- exp((64*coeff_dCCI) + (2506*coeff_dNDW) + (2902*coeff_dCRE) + intercept)
denominador <- 1 + numerador
probability <- numerador/denominador


#Predict raster
scene <- rast(paste0(path, "/level4/X0015_Y0024/2019_dVI.tif"))

numerador <- exp((scene$dCCI*coeff_dCCI) + (scene$dNDW*coeff_dNDW) + (scene$dCRE*coeff_dCRE) + intercept)
denominador <- 1 + numerador
probability <- numerador/denominador

#Mask application
mask <- values(scene$kND) >= 3000
values(scene$kND)[mask == TRUE] <- 1
values(scene$kND)[mask != TRUE] <- 0

#Apply mask
probability <- mask(probability, scene$kND, maskvalues = 0)


writeRaster(probability, paste0(path, "/level4/X0015_Y0024/2019_prob2.tif"), overwrite=TRUE)
