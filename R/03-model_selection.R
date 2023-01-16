################################################################################
##### 03 - Model selection
################################################################################

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(caret)
library(doParallel)
library(ggplot2)
library(viridis)

#-------------------------------------------------------------------------------
# Root path

path <- "/media/antonio/Work/Projects/Oak-wilt_mapping/level3_pixel-extraction"

#-------------------------------------------------------------------------------
# Reading and cleaning

#Reading -------------
data <- fread(paste0(path, "/master_training_normalized.csv"))
data <- na.exclude(data)

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
#X0014_Y0024 X0015_Y0024 X0016_Y0024 X0016_Y0025 X0016_Y0027 X0017_Y0024 X0017_Y0026 
#897         1178        937         218         96          238         77 

table(data_2019$Condition) 
#Dead Healthy  Wilted 
#1224    1194    1223 

datasplit_tile <- function(frame, training = 0.6) {
  
  frame <- frame
  frame$sample <- 1:nrow(frame)
  tiles <- unique(frame$tile)
  to_training <- as.numeric()
  
  for(i in 1:length(tiles)) {
    
    x_tile <- subset(frame, tile == tiles[i])
    min_samples <- min(table(x_tile$Condition))
    min_samples <- floor(min_samples * training)
    
    healthy <- subset(x_tile, Condition == "Healthy")
    symptomatic <- subset(x_tile, Condition == "Symptomatic")
    dead <- subset(x_tile, Condition == "Dead")
    
    healthy <- healthy[sample(1:nrow(healthy), min_samples), sample]
    symptomatic <- symptomatic[sample(1:nrow(symptomatic), min_samples), sample]
    dead <- dead[sample(1:nrow(dead), min_samples), sample]
    
    to_training <- c(to_training, healthy, symptomatic, dead)
    
  }
  
  return(to_training)
  
}

split <- datasplit_tile(frame = data_2019, training = 0.6)

#Datasplit 
condition_training <- data_2019[split,]
condition_testing <- data_2019[!split,]

#Export datasets
fwrite(condition_training, paste0(path, "/training_2019.csv"))
fwrite(condition_testing, paste0(path, "/testing_2019.csv"))

#-------------------------------------------------------------------------------
# Create a function of spatial fold
spatial_folds <- function(frame = condition_training, training = 0.80) {
  
  frame <- frame
  frame$sample <- 1:nrow(frame)
  tiles <- unique(frame$tile)
  
  folds <- list()
  
  for(i in 1:length(tiles)) {
    
    #Get the minimum of samples
    exclude <- subset(frame, tile != tiles[i])
    min_samples <- exclude[, .N, by = c("tile", "Condition")]
    min_samples <- min(min_samples$N)
    min_samples <- ceiling(min_samples * training)
    
    #Get a vector to keep folds
    to_training <- as.integer()
    
    #Tile of interest
    tiles_of_interest <- unique(exclude$tile)
    
    for(j in 1:length(tiles_of_interest)) {
      
      #Select tile
      x_tile <- subset(exclude, tile == tiles_of_interest[j])
      
      #Subset per condition
      healthy <- subset(x_tile, Condition == "Healthy")
      symptomatic <- subset(x_tile, Condition == "Symptomatic")
      dead <- subset(x_tile, Condition == "Dead")
      
      #Samples
      healthy <- healthy[sample(1:nrow(healthy), min_samples), sample]
      symptomatic <- symptomatic[sample(1:nrow(symptomatic), min_samples), sample]
      dead <- dead[sample(1:nrow(dead), min_samples), sample]
      
      to_training <- c(to_training, healthy, symptomatic, dead)
      
    }
    
    folds[[i]] <- to_training
    
  }
  
  names(folds) <- paste0("Fold", 1:length(folds))
  
  return(folds)
  
}

#-------------------------------------------------------------------------------
# Function for Model 

model_training <- function(data_model, 
                           repeated = 100, 
                           model, 
                           tune = NULL, 
                           threads = 4) {
  
  #Model collection
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
    
    #Folds
    folds <- spatial_folds(frame = condition_training)
    
    #Repeated 7-fold spatial cross-validation
    cfitControl <- trainControl(method = "cv",
                                number = 7,
                                index = folds,
                                classProbs = TRUE,
                                savePredictions = TRUE)
    
    #No tune
    if(is.null(tune)) {
      
      ml_model <- train(Condition ~ ., data = data_model, 
                        method = model, 
                        #preProcess = c("center", "scale"),
                        trControl = cfitControl)
      
      
    } else { #Use tune
      
      ml_model <- train(Condition ~ ., data = data_model, 
                        method= model, 
                        #preProcess = c("center", "scale"),
                        trControl = cfitControl,
                        tuneGrid = tune)
      
    }
    
    model_collector[[i]] <- ml_model
    
  }
  
  #Stop parallel
  if(threads > 1) {
    stopCluster(cl)
    gc()
  }
  
  return(model_collector)
  
}

#-------------------------------------------------------------------------------
# Find the 'best model'

#Read data
condition_training <- fread(paste0(path, "/training_2019.csv"))
condition_testing <- fread(paste0(path, "/testing_2019.csv"))

#Names for training
names <-  c("Condition", "VSS", "VGV", "VCV")
data_model <- condition_training[, ..names]

#Run LDA
lda <- model_training(data_model, 
                      repeated = 100,
                      model = "lda", 
                      tune = NULL, 
                      threads = 8)

#Run QDA
qda <- model_training(data_model, 
                      repeated = 100,
                      model = "qda", 
                      tune = NULL, 
                      threads = 8)

#Run RF
rf_tune <- expand.grid(mtry = c(1:10))
rf <- model_training(data_model, 
                     repeated = 100,
                     model = "rf", 
                     tune = rf_tune,
                     threads = 28)

#Run PLSD
pls_tune <- expand.grid(ncomp = c(1:3))
pls <- model_training(data_model, 
                      repeated = 100,
                      model = "pls", 
                      tune = pls_tune,
                      threads = 4)

#Run svmLinear
svmLinear_tune <- expand.grid(C = seq(0.1, 2, by = 0.1))
svmLinear <- model_training(data_model, 
                            repeated = 100,
                            model = "svmLinear", 
                            tune = svmLinear_tune,
                            threads = 28)

#Run Naive bayes
NB <- model_training(data_model, 
                     repeated = 100,
                     model = "nb", 
                     tune = NULL,
                     threads = 28)

#Run knn
knn_tune <- expand.grid(k = c(1:80))
knn <- model_training(data_model, 
                      repeated = 100,
                      model = "knn", 
                      tune = knn_tune,
                      threads = 28)

#Run nnet
nnet_tune <- expand.grid(size = seq(from = 1, to = 20, by = 1),
                         decay = seq(from = 0.1, to = 2, by = 0.1))
nnet <- model_training(data_model, 
                       repeated = 10,
                       model = "nnet", 
                       tune = nnet_tune,
                       threads = 28)

#Create a list of models
models <- list(LDA = lda, 
               QDA = qda,
               RF = rf, 
               PLSD = pls, 
               SVM = svmLinear, 
               NB = NB,
               KNN = knn, 
               NNET = nnet)

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

#-------------------------------------------------------------------------------
# Re-run models

#Run LDA
lda <- model_training(data_model, 
                      repeated = 100,
                      model = "lda", 
                      tune = NULL, 
                      threads = 8)

#Run QDA
qda <- model_training(data_model, 
                      repeated = 100,
                      model = "qda", 
                      tune = NULL, 
                      threads = 8)

#Run RF
rf_tune <- expand.grid(mtry = 1) #mtry equal to 1 seems to be the best
rf <- model_training(data_model, 
                     repeated = 100,
                     model = "rf", 
                     tune = rf_tune,
                     threads = 28)

#Run PLSD
pls_tune <- expand.grid(ncomp = 2) #ncomp equal to 2 seems to be the best
pls <- model_training(data_model, 
                      repeated = 100,
                      model = "pls", 
                      tune = pls_tune,
                      threads = 4)

#Run svmLinear
svmLinear_tune <- expand.grid(C = 1.5) #C equal to 1.5 seems to be the best 
svmLinear <- model_training(data_model, 
                            repeated = 100,
                            model = "svmLinear", 
                            tune = svmLinear_tune,
                            threads = 28)

#Run Naive bayes
NB <- model_training(data_model, 
                     repeated = 100,
                     model = "nb", 
                     tune = NULL,
                     threads = 28)

#Run knn
knn_tune <- expand.grid(k = 44) #k equal to 44 seems to be the best 
knn <- model_training(data_model, 
                      repeated = 100,
                      model = "knn", 
                      tune = knn_tune,
                      threads = 28)

#Run nnet
nnet_tune <- expand.grid(size = 15, #size equal to 15 seems to be the best 
                         decay = 1.21) #decay equal to 1.21 seems to be the best
nnet <- model_training(data_model, 
                       repeated = 100,
                       model = "nnet", 
                       tune = nnet_tune,
                       threads = 28)

#Create a list of models
models <- list(LDA = lda, 
               QDA = qda,
               RF = rf, 
               PLSD = pls, 
               SVM = svmLinear, 
               NB = NB,
               KNN = knn, 
               NNET = nnet)

#Export all model
saveRDS(models, "data/models/models.rds")

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

models <- readRDS("data/models/models.rds")

training_2019 <- training_performance(models = models, 
                                      dataset = condition_training[, ..names], 
                                      type = "Training",
                                      year = 2019,
                                      tile = "All")

testing_2019 <- testing_performance(models = models, 
                                    dataset = condition_testing[, ..names], 
                                    type = "Testing",
                                    year = 2019,
                                    tile = "All")

#Merge results
models_results <- rbind(training_2019, testing_2019[, c(1:7)])

#This is the file of results for figure 4
fwrite(models_results, "data/models_selection.csv")
