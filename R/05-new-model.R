################################################################################
##### 05 - Model (training - testing)
################################################################################

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(caret)
library(ggplot2)
library(viridis)
library(blockCV)


#-------------------------------------------------------------------------------
# Root path

path <- "F:/TRAINING/level3_lsf-pixels"
path <- "/media/antonio/antonio_ssd/TRAINING/level3_lsf-pixels"

#-------------------------------------------------------------------------------
# Reading and cleaning

#Reading -------------
data <- fread(paste0(path, "/master_observations.csv"))

#As factors
data$Condition <- as.factor(data$Condition)
data$Condition <- factor(data$Condition, levels = c("Healthy", "Wilted", "Dead"))

data$year <- as.factor(data$year)
data$year <- factor(data$year, levels = c("2018", "2019", "2021"))

#Unique combination of observations
remove <- na.exclude(data)
unique_IDs <- remove[, .N, by= c("tile", "ID", "Condition", "sensor")]
unique_IDs$N <- 1:nrow(unique_IDs)
data <- merge(unique_IDs, data, by = c("tile", "ID", "Condition", "sensor"), 
              all.x = TRUE, all.y = FALSE)
data <- data[order(N)]

#Look for area and year features
data_2019 <- data[year == "2019"]

#Data partitions ---------------------------------------------------------------

#Year of importance
data_2019 <- data[year == "2019"]

#Condition ---------------------------

table(data_2019$Condition) 
#Dead Healthy  Wilted 
#1091    1232    1258 

#Subset for splitting
healthy <- subset(data_2019,  Condition == "Healthy")
wilted <- subset(data_2019, Condition == "Wilted")
dead <- subset(data_2019, Condition == "Dead")

#Split
s_healthy <- createDataPartition(healthy$tile,
                                  p = 0.6,
                                  list = FALSE,
                                  times = 1)

s_wilted <- createDataPartition(wilted$tile,
                               p = 0.6,
                               list = FALSE,
                               times = 1)

s_dead <- createDataPartition(dead$tile,
                               p = 0.6,
                               list = FALSE,
                               times = 1)

condition_training <- rbind(healthy[s_healthy,], wilted[s_wilted,], dead[s_dead,])
condition_testing <- rbind(healthy[!s_healthy,], wilted[!s_wilted,], dead[!s_dead,])

#Names for training
names <-  c("Condition", "VAV", "VEV", "VGM", "VLV")

#-------------------------------------------------------------------------------
#Model

fit_model <- c("lda", "qda", "Linda", "treebag", "C5.0Tree")

model_training <- function(data_model, fit_model) {
  
  #Repeated 10-fold cross-validation
  cfitControl <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 10,
                              classProbs = TRUE,
                              savePredictions = TRUE)
  
  return_list <- list()
  
  for(i in 1:length(fit_model)) {
    
    #set.seed(825)
    ml_model <- train(Condition ~ ., data = data_model, 
                      method= fit_model[i], 
                      trControl = cfitControl)
    
    return_list[[i]] <- ml_model
    names(return_list)[[i]] <- fit_model[i]
    
  }
  
  return(return_list)
  
}

models <- model_training(condition_training[, ..names],
                         fit_model)

models <- model_training(condition_training[, ..names],
                         "C5.0Tree")

#Export model
saveRDS(models, "data/models/models.rds")

#-------------------------------------------------------------------------------
# Model Testing function

model_testing <- function(models, 
                          dataset,
                          type = "Training",
                          year = 2019) {
  
  #Number of models
  n_models <- length(models)
  
  #To future col names
  cm <- NA
  
  #Compile
  results_overal <- data.table()
  results_byClass <- data.table()
  
  for(i in 1:n_models) {
    
    model <- models[[i]]
    
    #Condition
    predicted <- data.frame(observed = as.factor(dataset$Condition),
                            predicted = predict(model, newdata = dataset))
    
    cm <- caret::confusionMatrix(predicted$observed, predicted$predicted)
    
    #Merge
    overal <- cbind(data.table(Model = names(models)[i], 
                               Type = type,
                               Year = year), 
                               matrix(cm$overall, nrow = 1))
    
    byClass <- cbind(data.table(Type = type,
                                Year = year,
                                Model = names(models)[i],
                                Class = rownames(cm$byClass)), 
                                cm$byClass)
    
    #Merge
    results_overal <- rbind(results_overal, overal)
    results_byClass <- rbind(results_byClass, byClass)
    
  }
  
  colnames(results_overal)[4:10] <- names(cm$overall)
  colnames(results_byClass)[5:15] <- colnames(cm$byClass)
  
  return(list(overal = results_overal, byClass = results_byClass))
  
}

#-------------------------------------------------------------------------------
#Validate models

training_2019 <- model_testing(models, 
                               dataset = condition_training[, ..names], 
                               type = "Training",
                               year = 2019)

testing_2019 <- model_testing(models, 
                              dataset = condition_testing[, ..names], 
                              type = "Testing",
                              year = 2019)

validation
