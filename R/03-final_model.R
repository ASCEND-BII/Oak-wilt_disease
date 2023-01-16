################################################################################
##### 03 - Final model (LDA)
################################################################################

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(caret)
library(doParallel)
library(multiROC)

#-------------------------------------------------------------------------------
# Root path

path <- "/media/antonio/Work/Projects/Oak-wilt_mapping/level3_pixel-extraction"

#-------------------------------------------------------------------------------
# Spatial cross-validation

#Read data
condition_training <- fread(paste0(path, "/training_2019.csv"))
condition_testing <- fread(paste0(path, "/testing_2019.csv"))

#Names for training
names <-  c("Condition", "VSS", "VGV", "VCV")
data_model <- condition_training[, ..names]

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
# Variance of the best model (LDA)
variance_training <- function(data_model, frame = condition_training, threads = 28, repeate = 10) {
  
  models <- list()
  
  #Run parallel
  if(threads > 1) {
    cl <- makePSOCKcluster(threads)
    registerDoParallel(cl)
  }

  for(i in 1:repeate) {
    
    #Group fold validation
    folds <- spatial_folds(frame = condition_training, training = 0.80)
    
    #Repeated 10-fold cross-validation
    cfitControl <- trainControl(method = "cv",
                                number = 7,
                                index = folds,
                                classProbs = TRUE,
                                savePredictions = TRUE)
    
    ml_model <- train(Condition ~ ., data = data_model, 
                      method= "lda", 
                      trControl = cfitControl)
    
    models[[i]] <- ml_model
    names(models)[i] <- paste0("LDA_", i)
    
  }
  
  #Stop parallel
  if(threads > 1) {
    stopCluster(cl)
    gc()
    Sys.sleep(3)
  }
  
  return(models)
  
}

final_model <- variance_training(data_model, 
                                 frame = condition_training, 
                                 threads = 28, 
                                 repeate = 10)

#Export final model
saveRDS(final_model, "data/models/final_model.rds")

#-------------------------------------------------------------------------------
# Model Testing Function
model_testing <- function(models, 
                          dataset,
                          type = "Training",
                          year = 2019,
                          tile = "All") {
  
  #Number of models
  n_models <- length(models)
  
  #To future col names
  cm <- NA
  
  #Compile
  results_overal <- data.table()
  results_byClass <- data.table()
  
  for(i in 1:n_models) {
    
    LDA <- models[i][[1]]
    
    #Condition
    predicted <- data.frame(observed = as.factor(dataset$Condition),
                            predicted = predict(LDA, newdata = dataset[, c(2:4)]))
    predicted$observed <- factor(predicted$observed, levels = c("Healthy", "Symptomatic", "Dead"))
    predicted$predicted <- factor(predicted$predicted, levels = c("Healthy", "Symptomatic", "Dead"))
    
    cm <- caret::confusionMatrix(predicted$observed, predicted$predicted)
    
    #Merge
    overal <- cbind(data.table(Model = names(models)[i], 
                               Tile = tile,
                               Type = type,
                               Year = year), 
                    matrix(cm$overall, nrow = 1))
    
    byClass <- cbind(data.table(Type = type,
                                Tile = tile,
                                Year = year,
                                Model = names(models)[i],
                                Class = rownames(cm$byClass)), 
                     cm$byClass)
    
    #Merge
    results_overal <- rbind(results_overal, overal)
    results_byClass <- rbind(results_byClass, byClass)
    
  }
  
  colnames(results_overal)[5:11] <- names(cm$overall)
  colnames(results_byClass)[6:16] <- colnames(cm$byClass)
  
  return(list(overal = results_overal, byClass = results_byClass))
  
}

#-------------------------------------------------------------------------------
# Model Testing 

final_model <- readRDS("data/models/final_model.rds")

#Read master file
data <- fread(paste0(path, "/master_training_normalized.csv"))
data <- na.exclude(data)

#As factors
data$Condition <- as.factor(data$Condition)
data$Condition <- factor(data$Condition, levels = c("Healthy", "Symptomatic", "Dead"))

data$dataset <- as.factor(data$dataset)
data$dataset <- factor(data$dataset, levels = c("2018", "2019", "2021"))

#Year of importance
data_2018 <- data[dataset == "2018"]
data_2021 <- data[dataset == "2021"]

#Predict
training_2019 <- model_testing(models = final_model, 
                              dataset = condition_training[, ..names], 
                              type = "Training",
                              year = 2019,
                              tile = "All")

testing_2019 <- model_testing(models = final_model, 
                              dataset = condition_testing[, ..names], 
                              type = "Testing",
                              year = 2019,
                              tile = "All")

testing_2018 <- model_testing(models = final_model, 
                              dataset = data_2018[, ..names], 
                              type = "Testing",
                              year = 2018,
                              tile = "All")

testing_2021 <- model_testing(models = final_model, 
                              dataset = data_2021[, ..names], 
                              type = "Testing",
                              year = 2021,
                              tile = "All")

#Spatial
X0014_Y0024_2019 <- model_testing(models = final_model, 
                                  dataset = condition_testing[tile == "X0014_Y0024", ..names], 
                                  type = "Testing",
                                  year = 2019,
                                  tile = "X0014_Y0024")

X0015_Y0024_2019 <- model_testing(models = final_model, 
                                  dataset = condition_testing[tile == "X0015_Y0024", ..names], 
                                  type = "Testing",
                                  year = 2019,
                                  tile = "X0015_Y0024")

X0016_Y0024_2019 <- model_testing(models = final_model, 
                                  dataset = condition_testing[tile == "X0016_Y0024", ..names], 
                                  type = "Testing",
                                  year = 2019,
                                  tile = "X0016_Y0024")

X0016_Y0025_2019 <- model_testing(models = final_model, 
                                  dataset = condition_testing[tile == "X0016_Y0025", ..names], 
                                  type = "Testing",
                                  year = 2019,
                                  tile = "X0016_Y0025")

X0016_Y0027_2019 <- model_testing(models = final_model, 
                                  dataset = condition_testing[tile == "X0016_Y0027", ..names], 
                                  type = "Testing",
                                  year = 2019,
                                  tile = "X0016_Y0027")

X0017_Y0024_2019 <- model_testing(models = final_model, 
                                  dataset = condition_testing[tile == "X0017_Y0024", ..names], 
                                  type = "Testing",
                                  year = 2019,
                                  tile = "X0017_Y0024")

X0017_Y0026_2019 <- model_testing(models = final_model, 
                                  dataset = condition_testing[tile == "X0017_Y0026", ..names], 
                                  type = "Testing",
                                  year = 2019,
                                  tile = "X0017_Y0026")


overal <- rbind(training_2019$overal,
                testing_2019$overal,
                testing_2018$overal,
                testing_2021$overal,
                X0014_Y0024_2019$overal,
                X0015_Y0024_2019$overal,
                X0016_Y0024_2019$overal,
                X0016_Y0025_2019$overal,
                X0016_Y0027_2019$overal,
                X0017_Y0024_2019$overal,
                X0017_Y0026_2019$overal)

byClass <- rbind(training_2019$byClass,
                 testing_2019$byClass,
                 testing_2018$byClass,
                 testing_2021$byClass,
                 X0014_Y0024_2019$byClass,
                 X0015_Y0024_2019$byClass,
                 X0016_Y0024_2019$byClass,
                 X0016_Y0025_2019$byClass,
                 X0016_Y0027_2019$byClass,
                 X0017_Y0024_2019$byClass,
                 X0017_Y0026_2019$byClass)

fwrite(overal, "data/models/overal.csv")
fwrite(byClass, "data/models/byClass.csv")

#-------------------------------------------------------------------------------
# ROC function

get_roc <- function(models = final_model, 
                    dataset = condition_training[, ..names], 
                    type = "Training",
                    year = 2019,
                    tile = "All") {
  
  #Number of models
  n_models <- length(models)
  
  #To future col names
  cm <- NA
  
  #Compile
  results <- data.table()
  
  for(i in 1:n_models) {
    
    #Get model
    LDA <- models[i][[1]]
    
    #Predict observations
    predict <- predict(LDA, dataset, type = 'prob') 
    colnames(predict) <- paste0(colnames(predict), "_pred_", tile)
    
    #Frame
    true_label <- dummies::dummy(dataset$Condition, sep = ".")
    true_label <- data.frame(true_label)
    colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
    colnames(true_label) <- paste0(colnames(true_label), "_true")
    final <- cbind(true_label, predict)
    
    #Get ROC
    roc_res <- multi_roc(final, force_diag=T)
    plot_roc <- plot_roc_data(roc_res)
    colnames(plot_roc)[3] <- "Condition"
    plot_roc$Condition <- as.factor(plot_roc$Condition)
    plot_roc$Condition <- factor(plot_roc$Condition, levels = c("Healthy", 
                                                                "Symptomatic", 
                                                                "Dead", 
                                                                "Macro", 
                                                                "Micro"))
    
    plot_roc$type <- type
    plot_roc$year <- year
    plot_roc$tile <- tile 
    plot_roc$repedition <- names(models[i])
    
    #Collect results
    results <- rbind(results, plot_roc)
    
  }
  
  return(results)
  
}

#-------------------------------------------------------------------------------
# Get ROCs application

training_2019_roc <- get_roc(models = final_model, 
                             dataset = condition_training[, ..names], 
                             type = "Training",
                             year = 2019,
                             tile = "All")

testing_2019_roc <- get_roc(models = final_model, 
                            dataset = condition_testing[, ..names], 
                            type = "Testing",
                            year = 2019,
                            tile = "All")

testing_2018_roc <- get_roc(models = final_model, 
                            dataset = data_2018[, ..names], 
                            type = "Testing",
                            year = 2018,
                            tile = "All")

testing_2021_roc <- get_roc(models = final_model, 
                            dataset = data_2021[, ..names], 
                            type = "Testing",
                            year = 2021,
                            tile = "All")

#Spatial
X0014_Y0024_2019_roc <- get_roc(models = final_model, 
                                dataset = condition_testing[tile == "X0014_Y0024", ..names], 
                                type = "Testing",
                                year = 2019,
                                tile = "X0014_Y0024")

X0015_Y0024_2019_roc <- get_roc(models = final_model, 
                                dataset = condition_testing[tile == "X0015_Y0024", ..names], 
                                type = "Testing",
                                year = 2019,
                                tile = "X0015_Y0024")

X0016_Y0024_2019_roc <- get_roc(models = final_model, 
                                dataset = condition_testing[tile == "X0016_Y0024", ..names], 
                                type = "Testing",
                                year = 2019,
                                tile = "X0016_Y0024")

X0016_Y0025_2019_roc <- get_roc(models = final_model, 
                                dataset = condition_testing[tile == "X0016_Y0025", ..names], 
                                type = "Testing",
                                year = 2019,
                                tile = "X0016_Y0025")

X0016_Y0027_2019_roc <- get_roc(models = final_model, 
                                dataset = condition_testing[tile == "X0016_Y0027", ..names], 
                                type = "Testing",
                                year = 2019,
                                tile = "X0016_Y0027")

X0017_Y0024_2019_roc <- get_roc(models = final_model, 
                                dataset = condition_testing[tile == "X0017_Y0024", ..names], 
                                type = "Testing",
                                year = 2019,
                                tile = "X0017_Y0024")

X0017_Y0026_2019_roc <- get_roc(models = final_model, 
                                dataset = condition_testing[tile == "X0017_Y0026", ..names], 
                                type = "Testing",
                                year = 2019,
                                tile = "X0017_Y0026")

results_roc <- rbind(training_2019_roc,
                     testing_2019_roc,
                     testing_2018_roc,
                     testing_2021_roc,
                     X0014_Y0024_2019_roc,
                     X0015_Y0024_2019_roc,
                     X0016_Y0024_2019_roc,
                     X0016_Y0025_2019_roc,
                     X0016_Y0027_2019_roc,
                     X0017_Y0024_2019_roc,
                     X0017_Y0026_2019_roc)

fwrite(results_roc, "data/models/rocs.csv")
