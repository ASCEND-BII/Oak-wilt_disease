################################################################################
##### 05 - Model (training - testing)
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

path <- "F:/TRAINING/level3_lsf-pixels"
path <- "/media/antonio/antonio_ssd/TRAINING/level3_lsf-pixels"

#-------------------------------------------------------------------------------
# Reading and cleaning

#Reading -------------
data <- fread(paste0(path, "/master_training.csv"))

#As factors
data$Condition <- as.factor(data$Condition)
data$Condition <- factor(data$Condition, levels = c("Healthy", "Wilted", "Dead"))

data$year <- as.factor(data$year)
data$year <- factor(data$year, levels = c("2018", "2019", "2021"))

#Data partitions ---------------------------------------------------------------

#Year of importance
data_2019 <- data[year == "2019"]
data_2018 <- data[year == "2018"]
data_2021 <- data[year == "2021"]

#Condition ---------------------------

table(data_2019$Condition) 
#Dead Healthy  Wilted 
#1120    1177    998 

#Subset for splitting
healthy <- subset(data_2019,  Condition == "Healthy")
wilted <- subset(data_2019, Condition == "Wilted")
dead <- subset(data_2019, Condition == "Dead")

#Split
set.seed(825)
s_healthy <- createDataPartition(healthy$tile,
                                  p = 0.6,
                                  list = FALSE,
                                  times = 1)

set.seed(825)
s_wilted <- createDataPartition(wilted$tile,
                               p = 0.6,
                               list = FALSE,
                               times = 1)

set.seed(825)
s_dead <- createDataPartition(dead$tile,
                               p = 0.6,
                               list = FALSE,
                               times = 1)

condition_training <- rbind(healthy[s_healthy,], wilted[s_wilted,], dead[s_dead,])
condition_testing <- rbind(healthy[!s_healthy,], wilted[!s_wilted,], dead[!s_dead,])

#Export datasets
fwrite(condition_training, paste0(path, "/training_2019.csv"))
fwrite(condition_testing, paste0(path, "/testing_2019.csv"))

#Names for training
names <-  c("Condition", "PPM", "VSS", "VCV", "IFR")

#-------------------------------------------------------------------------------
# Function for Model

model_training <- function(data_model, model, tune = NULL, threads = 4) {
  
  #Repeated 10-fold cross-validation
  cfitControl <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 10,
                              classProbs = TRUE,
                              savePredictions = TRUE)
  
  #Run parallel
  if(threads > 1) {
    cl <- makePSOCKcluster(threads)
    registerDoParallel(cl)
  }
  
  #No tune
  if(is.null(tune)) {
    
    set.seed(825)
    ml_model <- train(Condition ~ ., data = data_model, 
                      method= model, 
                      trControl = cfitControl)
    
    
  } else { #Use tune
    
    set.seed(825)
    ml_model <- train(Condition ~ ., data = data_model, 
                      method= model, 
                      trControl = cfitControl,
                      tuneGrid = tune)
    
  }
  
  #Stop parallel
  if(threads > 1) {
    stopCluster(cl)
  }
  
  return(ml_model)
  
}

data_model <- condition_training[, ..names]

#-------------------------------------------------------------------------------
# Find the 'best model'

#Run LDA
lda <- model_training(data_model, 
                      model = "lda", 
                      tune = NULL, 
                      threads = 8)

#Run RF
rf_tune <- expand.grid(mtry = c(1:10))
rf <- model_training(data_model, 
                     model = "rf", 
                     tune = rf_tune,
                     threads = 25)

#Run PLSD
pls_tune <- expand.grid(ncomp = c(1:4))
pls <- model_training(data_model, 
                      model = "pls", 
                      tune = pls_tune,
                      threads = 4)

#Run svmLinear
svmLinear_tune <- expand.grid(C = seq(0.1, 2, by = 0.1))
svmLinear <- model_training(data_model, 
                            model = "svmLinear", 
                            tune = svmLinear_tune,
                            threads = 16)

#Run Naive bayes
NB <- model_training(data_model, 
                     model = "nb", 
                     tune = NULL,
                     threads = 16)

#Run knn
knn_tune <- expand.grid(k = c(1:30))
knn <- model_training(data_model, 
                      model = "knn", 
                      tune = knn_tune,
                      threads = 25)

#Run nnet
nnet_tune <- expand.grid(size = seq(from = 1, to = 10, by = 1),
                         decay = seq(from = 0.1, to = 0.5, by = 0.1))
nnet <- model_training(data_model, 
                       model = "nnet", 
                       tune = nnet_tune,
                       threads = 25)

#Create a list of models
models <- list(LDA = lda, 
               RF = rf, 
               PLSD = pls, 
               SVM = svmLinear, 
               NB = NB,
               KNN = knn, 
               NNET = nnet)

resamps <- resamples(models, 
                     metric = c("Accuracy"))
summary(resamps)


theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)

jpeg("mode-performance.jpeg", quality = 100, res = 300, width = 210, height = 80, units = "mm", pointsize = 12) # JPEG device

bwplot(resamps, layout = c(2, 1))

dev.off()


#Export model
saveRDS(models, "data/models/models.rds")

#-------------------------------------------------------------------------------
# Variance of the best model (SVM)

variance_training <- function(data_model, threads = 26, repeats = 10) {
  
  #Repeated 10-fold cross-validation
  cfitControl <- trainControl(method = "cv",
                              number = 10,
                              classProbs = TRUE,
                              savePredictions = TRUE)
  
  models <- list()
  
  for(i in 1:repeats) {
    
    #Run parallel
    if(threads > 1) {
      cl <- makePSOCKcluster(threads)
      registerDoParallel(cl)
    }
    
    tune <- expand.grid(C = 0.7)
    ml_model <- train(Condition ~ ., data = data_model, 
                      method= "svmLinear", 
                      trControl = cfitControl,
                      tuneGrid = tune)
    
    models[[i]] <- ml_model
    names(models)[i] <- paste0("SVM_", i)
    
  }
  
  #Stop parallel
  if(threads > 1) {
    stopCluster(cl)
    Sys.sleep(3)
  }
  
  return(models)

}

final_model <- variance_training(data_model, threads = 26, repeats = 10)

#Export final model
saveRDS(final_model, "data/models/final_model.rds")

#-------------------------------------------------------------------------------
# Model Testing function

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
    
    model <- models[[i]]
    
    #Condition
    predicted <- data.frame(observed = as.factor(dataset$Condition),
                            predicted = predict(model, newdata = dataset))
    
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
#Validate models

#Temporal
training_2019 <- model_testing(final_model, 
                               dataset = condition_training[, ..names], 
                               type = "Training",
                               year = 2019,
                               tile = "All")

testing_2019 <- model_testing(final_model, 
                              dataset = condition_testing[, ..names], 
                              type = "Testing",
                              year = 2019,
                              tile = "All")

testing_2018 <- model_testing(final_model, 
                              dataset = data_2018[, ..names], 
                              type = "Testing",
                              year = 2018,
                              tile = "All")

testing_2021 <- model_testing(final_model, 
                              dataset = data_2021[, ..names], 
                              type = "Testing",
                              year = 2021,
                              tile = "All")

#Spatial
X0014_Y0024_2019 <- model_testing(final_model, 
                                  dataset = condition_testing[tile == "X0014_Y0024", ..names], 
                                  type = "Testing",
                                  year = 2019,
                                  tile = "X0014_Y0024")

X0015_Y0024_2019 <- model_testing(final_model, 
                                  dataset = condition_testing[tile == "X0015_Y0024", ..names], 
                                  type = "Testing",
                                  year = 2019,
                                  tile = "X0015_Y0024")

X0016_Y0024_2019 <- model_testing(final_model, 
                                  dataset = condition_testing[tile == "X0016_Y0024", ..names], 
                                  type = "Testing",
                                  year = 2019,
                                  tile = "X0016_Y0024")

X0016_Y0025_2019 <- model_testing(final_model, 
                                  dataset = condition_testing[tile == "X0016_Y0025", ..names], 
                                  type = "Testing",
                                  year = 2019,
                                  tile = "X0016_Y0025")

X0016_Y0027_2019 <- model_testing(final_model, 
                                  dataset = condition_testing[tile == "X0016_Y0027", ..names], 
                                  type = "Testing",
                                  year = 2019,
                                  tile = "X0016_Y0027")

X0017_Y0024_2019 <- model_testing(final_model, 
                                  dataset = condition_testing[tile == "X0017_Y0024", ..names], 
                                  type = "Testing",
                                  year = 2019,
                                  tile = "X0017_Y0024")

X0017_Y0026_2019 <- model_testing(final_model, 
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
