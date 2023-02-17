################################################################################
##### 03 - Model selection
################################################################################

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(caret)
library(CAST)
library(doParallel)

#-------------------------------------------------------------------------------
# Source code
source("R/00-data_split.R")

#-------------------------------------------------------------------------------
# Root path

path <- "/media/antonio/Work/Projects/Oak-wilt_mapping/level3_pixel-extraction"

#-------------------------------------------------------------------------------
# Reading and cleaning

#Read
data <- fread(paste0(path, "/master_normalized.csv"))

#As factors
data$Condition <- as.factor(data$Condition)
data$Condition <- factor(data$Condition, levels = c("Healthy", "Symptomatic", "Dead"))

data$dataset <- as.factor(data$dataset)
data$dataset <- factor(data$dataset, levels = c("2018", "2019", "2021"))

# Data partitions --------------------------------------------------------------

# Year of importance
data_2019 <- data[dataset == "2019"]
data_2018 <- data[dataset == "2018"]
data_2021 <- data[dataset == "2021"]

# n of samples
table(data_2019$tile) 
#X0014_Y0024 X0015_Y0024 X0016_Y0024 X0016_Y0025 X0016_Y0027 X0017_Y0024 X0017_Y0026 X0017_Y0027
#895         1176        937         218         96          238         101                 107

table(data_2019$Condition) 
#Dead Healthy  Wilted 
#1267    1234    1267 

#Data split
split <- datasplit_tile(frame = data_2019, training = 0.6)

#Datasplit 
condition_training <- data_2019[split,]
condition_testing <- data_2019[!split,]

#Export datasets
fwrite(condition_training, paste0(path, "/training_2019.csv"))
fwrite(condition_testing, paste0(path, "/testing_2019.csv"))

#-------------------------------------------------------------------------------
# Function for Model 

#Create folds and split -----------------------
#Model collectors
data_split <- list()
data_folds <- list()

#N of iterations
repeated <- 100

#Proportion of data split
prop <- 0.8

for(i in 1:repeated) {
  
  #Split
  split <- datasplit_tile(condition_training, prop)
  
  #Folds
  folds <- CreateSpacetimeFolds(x = condition_training[split],
                                spacevar = "tile", 
                                k = 8,
                                seed = sample(1:10000, 1))
  
  #Collect
  data_split[[i]] <- split
  data_folds[[i]] <- folds
  
  #Residuals
  rm(list = c("split", "folds"))
  
}

model_training <- function(data, 
                           col_names = c("Condition", "VSS", "VGV", "VCV"),
                           model, 
                           tune = NULL, 
                           repeated = 100,
                           data_split = data_split,
                           data_folds = data_folds,
                           threads = 4) {
  
  #Model collector
  model_collector <- list()
  
  #Progress bar
  pb <- txtProgressBar(min = 0, max = repeated, style = 3)
  
  #Run parallel
  if(threads > 1) {
    cl <- makePSOCKcluster(threads)
    registerDoParallel(cl)
  }
  
  for(i in 1:repeated) {
    
    #Progress
    setTxtProgressBar(pb, i)
    
    folds <- data_folds[[i]]$index
    
    #Repeated 8-fold spatial cross-validation
    cfitControl <- trainControl(method = "cv",
                                number = 8,
                                index = folds,
                                classProbs = TRUE,
                                savePredictions = TRUE)
    
    #No tune
    if(is.null(tune)) {
      
      ml_model <- train(Condition ~ ., 
                        data = data[data_split[[i]], ..col_names], 
                        method = model, 
                        trControl = cfitControl)

    } else { #Use tune
      
      ml_model <- train(Condition ~ ., 
                        data = data[data_split[[i]], ..col_names], 
                        method= model, 
                        trControl = cfitControl,
                        tuneGrid = tune)
      
    }
    
    model_collector[[i]] <- ml_model
    
  }
  
  #Stop parallel
  if(threads > 1) {
    stopCluster(cl)
    Sys.sleep(2)
    gc()
  }
  
  return(model_collector)
  
}

#-------------------------------------------------------------------------------
# Find the 'best model'

#Read data
condition_training <- fread(paste0(path, "/training_2019.csv"))
condition_testing <- fread(paste0(path, "/testing_2019.csv"))

predictors <- c("Condition", "VSS", "VES", "VCV")

#Run LDA
lda <- model_training(data = condition_training,
                      col_names = predictors,
                      model = "lda", 
                      tune = NULL,
                      repeated = 100,
                      data_split = data_split,
                      data_folds = data_folds,
                      threads = 1)

#Run QDA
qda <- model_training(data = condition_training,
                      col_names = predictors,
                      model = "qda", 
                      tune = NULL,
                      repeated = 100,
                      data_split = data_split,
                      data_folds = data_folds,
                      threads = 1)

#Run RF
rf_tune <- expand.grid(mtry = c(1:10))
rf <- model_training(data = condition_training,
                     col_names = predictors,
                     model = "rf",
                     tune = rf_tune,
                     repeated = 100,
                     data_split = data_split,
                     data_folds = data_folds,
                     threads = 28)

#Run PLSD
pls_tune <- expand.grid(ncomp = c(1:3))
pls <- model_training(data = condition_training,
                      col_names = predictors, 
                      model = "pls", 
                      tune = pls_tune,
                      repeated = 100,
                      data_split = data_split,
                      data_folds = data_folds,
                      threads = 24)

#Run svmLinear
svmLinear_tune <- expand.grid(C = seq(0.1, 2, by = 0.1))
svmLinear <- model_training(data = condition_training,
                            col_names = predictors,
                            model = "svmLinear", 
                            tune = svmLinear_tune,
                            repeated = 100,
                            data_split = data_split,
                            data_folds = data_folds,
                            threads = 24)

#Run knn
knn_tune <- expand.grid(k = c(1:100))
knn <- model_training(data = condition_training,
                      col_names = predictors,
                      model = "knn",
                      tune = knn_tune,
                      repeated = 100,
                      data_split = data_split,
                      data_folds = data_folds,
                      threads = 24)

#Create a list of models
models <- list(LDA = lda, 
               QDA = qda,
               RF = rf, 
               PLSD = pls, 
               SVM = svmLinear, 
               KNN = knn)

#-------------------------------------------------------------------------------
# Get best tuning parameter to re-run

best_tuning <- function(models) {
  
  #Number of models
  n_models <- length(models)
  
  #To future col names
  cm <- NA
  
  #Collector
  tuning <- data.table()
  
  for(i in 1:n_models) {
    
    model <- models[[i]]
    
    for(ii in 1:length(model)) {
      
      sub_model <- model[[ii]]
      
      tune <- sub_model$bestTune
      
      if(tune[1] == "none") {
        tune <- NA
      } else {
        tune <- as.numeric(tune)
      }
      
      #Merge
      overal <- cbind(data.table(Model = names(models)[i], 
                                 Repetition = ii,
                                 Tune_1 = tune[1],
                                 Tune_2 = tune[2]))
      
      #Merge
      tuning <- rbind(tuning, overal)
      
    }
  }
  
  return(tuning)
  
}

#Explore best models
best <- best_tuning(models)

#Mode function
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

#Get the mode
best[, find_mode(Tune_1), by = "Model"]

#Export best turning
fwrite(best, "data/best-turning.csv")

#-------------------------------------------------------------------------------
# Re-run models

#Run LDA
lda <- model_training(data = condition_training,
                      col_names = predictors,
                      model = "lda", 
                      tune = NULL,
                      repeated = 100,
                      data_split = data_split,
                      data_folds = data_folds,
                      threads = 1)

#Run QDA
qda <- model_training(data = condition_training,
                      col_names = predictors,
                      model = "qda", 
                      tune = NULL,
                      repeated = 100,
                      data_split = data_split,
                      data_folds = data_folds,
                      threads = 1)

#Run RF
rf_tune <- expand.grid(mtry = 1) #mtry equal to 1 seems to be the best
rf <- model_training(data = condition_training,
                     col_names = predictors,
                     model = "rf",
                     tune = rf_tune,
                     repeated = 100,
                     data_split = data_split,
                     data_folds = data_folds,
                     threads = 28)

#Run PLSD
pls_tune <- expand.grid(ncomp = 3) #ncomp equal to 3 seems to be the best
pls <- model_training(data = condition_training,
                      col_names = predictors, 
                      model = "pls", 
                      tune = pls_tune,
                      repeated = 100,
                      data_split = data_split,
                      data_folds = data_folds,
                      threads = 3)

#Run svmLinear
svmLinear_tune <- expand.grid(C = 0.5) #C equal to 0.5 seems to be the best 
svmLinear <- model_training(data = condition_training,
                            col_names = predictors,
                            model = "svmLinear", 
                            tune = svmLinear_tune,
                            repeated = 100,
                            data_split = data_split,
                            data_folds = data_folds,
                            threads = 24)

#Run knn
knn_tune <- expand.grid(k = 34) #k equal to 34 seems to be the best 
knn <- model_training(data = condition_training,
                      col_names = predictors,
                      model = "knn",
                      tune = knn_tune,
                      repeated = 100,
                      data_split = data_split,
                      data_folds = data_folds,
                      threads = 34)

#Create a list of models
models <- list(LDA = lda, 
               QDA = qda,
               RF = rf, 
               PLSD = pls, 
               SVM = svmLinear, 
               KNN = knn)

#-------------------------------------------------------------------------------
# Training Performance
training_performance <- function(models, 
                                 dataset,
                                 type = "Training",
                                 year = 2019,
                                 tile = "All") {
  
  #Number of models
  n_models <- length(models)
  
  #To future col names
  cm <- NA
  
  #Collector
  results_overal <- data.table()
  
  for(i in 1:n_models) {
    
    model <- models[[i]]
    
    for(ii in 1:length(model)) {
      
      sub_model <- model[[ii]]
      
      #Merge
      overal <- data.table(Model = names(models)[i], 
                           Repetition = ii,
                           Tile = tile,
                           Type = type,
                           Year = year,
                           Accuracy = sub_model$results$Accuracy,
                           Kappa = sub_model$results$Kappa)
      
      #Merge
      results_overal <- rbind(results_overal, overal)
      
    }
  }
  
  return(results_overal)
  
}

# Testing performance
testing_performance <- function(models, 
                                dataset,
                                type = "Training",
                                year = 2019,
                                tile = "All") {
  
  #Number of models
  n_models <- length(models)
  
  #To future col names
  cm <- NA
  
  #Collector
  results_overal <- data.table()
  
  for(i in 1:n_models) {
    
    model <- models[[i]]
    
    for(ii in 1:length(model)) {
      
      sub_model <- model[[ii]]
      
      #Condition
      predicted <- data.frame(observed = as.factor(dataset$Condition),
                              predicted = predict(sub_model, newdata = dataset))
      predicted$observed <- factor(predicted$observed, levels = c("Healthy", "Symptomatic", "Dead"))
      predicted$predicted <- factor(predicted$predicted, levels = c("Healthy", "Symptomatic", "Dead"))
      
      cm <- caret::confusionMatrix(predicted$observed, predicted$predicted)
      
      #Merge
      overal <- cbind(data.table(Model = names(models)[i], 
                                 Repetition = ii,
                                 Tile = tile,
                                 Type = type,
                                 Year = year), 
                      matrix(cm$overall, nrow = 1))
      
      #Merge
      results_overal <- rbind(results_overal, overal)
      
    }
  }
  
  colnames(results_overal)[6:12] <- names(cm$overall)
  
  return(results_overal)
  
}

#-------------------------------------------------------------------------------
#Validate models

training_2019 <- training_performance(models = models, 
                                      dataset = condition_training[, ..predictors], 
                                      type = "Training",
                                      year = 2019,
                                      tile = "All")

testing_2019 <- testing_performance(models = models, 
                                    dataset = condition_testing[, ..predictors], 
                                    type = "Testing",
                                    year = 2019,
                                    tile = "All")

#Merge results
models_results <- rbind(training_2019, testing_2019[, c(1:7)])

#This is the file of results for figure 4
fwrite(models_results, "data/models_selection.csv")

#Export all model
saveRDS(models, "data/models/models.rds")
