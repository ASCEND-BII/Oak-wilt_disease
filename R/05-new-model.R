################################################################################
##### 05 - Model (training - testing)
################################################################################

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(caret)
library(ggplot2)
library(viridis)

#-------------------------------------------------------------------------------
# Root path

path <- "/media/antonio/antonio_ssd/TRAINING/level3_phenology-pixels"

#-------------------------------------------------------------------------------
# Reading and cleaning

#Reading -------------
X0014_Y0024 <- fread(paste0(path, "/X0014_Y0024_phenology.txt"))
X0015_Y0024 <- fread(paste0(path, "/X0015_Y0024_phenology.txt"))
X0016_Y0024 <- fread(paste0(path, "/X0016_Y0024_phenology.txt"))
X0016_Y0025 <- fread(paste0(path, "/X0016_Y0025_phenology.txt"))
X0016_Y0027 <- fread(paste0(path, "/X0016_Y0027_phenology.txt"))
X0017_Y0026 <- fread(paste0(path, "/X0017_Y0026_phenology.txt"))
X0017_Y0027 <- fread(paste0(path, "/X0017_Y0027_phenology.txt"))

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
data <- data[, c(9, 1:8)]

#Look for area and year features
data <- data[year == "2019"]

#Unique combination
remove <- na.exclude(data)
unique_IDs <- remove[, .N, by= c("tile", "ID", "Condition")]
unique_IDs$N <- 1:nrow(unique_IDs)

#Final to use
data <- merge(unique_IDs, data, by = c("tile", "ID", "Condition"), 
              all.x = TRUE, all.y = FALSE)

#Dcast
data <- dcast(data, formula = N + tile + ID + x + y + Condition + year + VI ~ metric)

#Select columns of importance
data <- data[, c("N", "tile", "ID", "x", "y", "Condition", "year", "VI",
                 "VEM", "VSS", "VPS", "VMS", "VES", "VLM", "VEV", "VAV", "VLV", "VBL", "VSA", "VGA", "VPA", "VGM", "VGV", 
                 "IST", "IBL", "IBT", "IGS", "IRR",
                 "IFR", "RAR", "RAF", "RMR", "RMF")]

#Use just clean observations
clean_data <- fread("/media/antonio/antonio_ssd/TRAINING/level3_ts-pixels/clean_samples.csv")

#Samples clean
data <- merge(clean_data, data, by = c("x", "y", "Condition"), all.x = TRUE)

fwrite(data, paste0(path, "/master_observations.csv"))

#-------------------------------------------------------------------------------
# New metrics
data$VVR <- data$VLV/data$VEV
data$PPV <- data$VPA/data$VGM
data$CVG <- data$VGV/data$VGM
data$CVG <- data$VGV/data$VGM


data$PIT <- (data$IBT - data$IST)/data$IBT
data$S_proportion <- (test$VSS - test$VES)/test$VSS
data$R_proportion <- test$IFR/test$IBT



data <- fread(paste0(path, "/master_observations.csv"))

ggplot(data[VI == "CCI" & VPA > 0], aes(x= Condition, y= IFR, fill = Condition)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)

ggplot(data[VI == "CCI" & VPA > 0], aes(x= Condition, y= VPA/10000, fill = Condition)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)

ggplot(data[VI == "CCI" & VPA > 0], aes(x= Condition, y= VPS/10000, fill = Condition)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)

ggplot(data[VI == "CCI" & VPA > 0], aes(x= Condition, y= VGV/10000, fill = Condition)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)

ggplot(data[VI == "CCI" & VPA > 0], aes(x= Condition, y= IGS/10000, fill = Condition)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)

ggplot(data[VI == "CCI" & VPA > 0], aes(x= Condition, y= IFR, fill = Condition)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)

ggplot(data[VI == "CCI" & VPA > 0], aes(x= Condition, y= VLV/VEV, fill = Condition)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)

ggplot(data[VI == "CCI" & VPA > 0], aes(x= Condition, y= VPA/VPS, fill = Condition)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)

ggplot(data[VI == "CCI" & VPA > 0], aes(x= Condition, y= ((VSS-VES)/VSS), fill = Condition)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  scale_y_continuous(limits = c(0, 1))

ggplot(data[VI == "CCI" & VPA > 0], aes(x= Condition, y= (IBT-IST)/IBT, fill = Condition)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) 

ggplot(data[VI == "CCI" & VPA > 0], aes(x= Condition, y= ((VSS-VES)/VSS), fill = Condition)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  scale_y_continuous(limits = c(-1, 1))

ggplot(data[VI == "CCI" & VPA > 0], aes(x= Condition, y= (IFR/IBT), fill = Condition)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)


#Data partitions ---------------------------------------------------------------
#Condition ---------------------------

#data <- fread("master.csv")
#data <- data[VI == "CCI"]

table(data_kNDVI$Condition) #625 529 

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
  cfitControl <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 10,
                              classProbs = TRUE,
                              savePredictions = TRUE)
  
  #set.seed(825)
  condition_model <- train(condition ~ ., data = condition_training, 
                           method="lda", 
                           trControl = cfitControl,
                           weights = condition_weights * 100)
  
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
  
  return(list(condition = condition_model,
              wilted = wilted_model,
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
                          condition_training, condition_testing,
                          wilted_training, wilted_testing, 
                          healthy_training, healthy_testing, 
                          dead_training, dead_testing) {
  
  #Condition
  condition_training_m <- data.frame(observed = as.factor(condition_training$condition),
                                     predicted = predict(models$condition))
  
  condition_testing_m <- data.frame(observed = as.factor(condition_testing$condition),
                                    predicted = predict(models$condition, newdata = condition_testing))
  
  condition_training_cm <- caret::confusionMatrix(condition_training_m$observed, condition_training_m$predicted)
  condition_testing_cm <- caret::confusionMatrix(condition_testing_m$observed, condition_testing_m$predicted)
  
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
  condition_training <- cbind(data.table(Condition = "Condition", Step = "Training"), 
                              matrix(condition_training_cm$overall, nrow = 1), 
                              matrix(condition_training_cm$byClass, nrow = 1))
  
  condition_testing <- cbind(data.table(Condition = "Condition", Step = "Testing"), 
                          matrix(condition_testing_cm$overall, nrow = 1), 
                          matrix(condition_testing_cm$byClass, nrow = 1))
  
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
  
  lda <- rbind(condition_training, condition_testing)
  colnames(lda)[3:9] <- names(condition_training_cm$overall)
  #colnames(frame)[10:20] <- names(condition_training_cm$byClass)
  
  frame <- rbind(wilted_training, wilted_testing,
                 healthy_training, healthy_testing,
                 dead_training, dead_testing)
  
  colnames(frame)[3:9] <- names(dead_training_cm$overall)
  colnames(frame)[10:20] <- names(dead_training_cm$byClass)
  
  return(list(lda = lda, bglm = frame))
  
}

validation <- model_testing(models, 
                            condition_training, condition_testing,
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
