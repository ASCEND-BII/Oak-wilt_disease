################################################################################
##### 03 - Final model assesement (PLSD)
################################################################################

#' @description A script for extracting important metrics of performance and 
#' coefficients in the final selected model.

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(caret)
library(doParallel)
library(multiROC)
library(ROCR)

#-------------------------------------------------------------------------------
# Load source code
source("R/00-cutoff_ROC.R")

#-------------------------------------------------------------------------------
# Root path

path <- "/media/antonio/Work/Projects/Oak-wilt_mapping/level3_pixel-extraction"

#-------------------------------------------------------------------------------
# Read datasets and models

#Read data ------------------------------------
condition_training <- fread(paste0(path, "/training_2019.csv"))
condition_testing <- fread(paste0(path, "/testing_2019.csv"))

#Read master file -----------------------------
data <- fread(paste0(path, "/master_normalized.csv"))
data <- na.exclude(data)

#As factors
data$Condition <- as.factor(data$Condition)
data$Condition <- factor(data$Condition, levels = c("Healthy", "Symptomatic", "Dead"))

data$dataset <- as.factor(data$dataset)
data$dataset <- factor(data$dataset, levels = c("2018", "2019", "2021"))

#Year of importance
data_2018 <- data[dataset == "2018"]
data_2021 <- data[dataset == "2021"]

#Read models ---------------------------------
models <- readRDS("data/models/models.rds")
MOD <- models$PLSD

#-------------------------------------------------------------------------------
# Model Predict Function
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
    
    PLSD <- models[[i]]
    
    #Condition
    predicted <- data.frame(observed = as.factor(dataset$Condition),
                            predicted = predict(PLSD, newdata = dataset[, c(2:4)]))
    predicted$observed <- factor(predicted$observed, levels = c("Healthy", "Symptomatic", "Dead"))
    predicted$predicted <- factor(predicted$predicted, levels = c("Healthy", "Symptomatic", "Dead"))
    
    cm <- caret::confusionMatrix(predicted$observed, predicted$predicted, mode = "everything")
    
    #Merge
    overal <- cbind(data.table(Tile = tile,
                               Type = type,
                               Year = year,
                               iteration = i), 
                    matrix(cm$overall, nrow = 1))
    
    byClass <- cbind(data.table(Tile = tile,
                                Type = type,
                                Year = year,
                                iteration = i,
                                Class = rownames(cm$byClass)), 
                     cm$byClass)
    
    #Merge
    results_overal <- rbind(results_overal, overal)
    results_byClass <- rbind(results_byClass, byClass)
    
  }
  
  colnames(results_overal) <- c("Tile", "Type", "Year", "iteration",
                                names(cm$overall))

  #Change names
  results_byClass[Class == "Class: Healthy", Class := "Healthy"]
  results_byClass[Class == "Class: Symptomatic", Class := "Symptomatic"]
  results_byClass[Class == "Class: Dead", Class := "Dead"]
  
  return(list(overal = results_overal, byClass = results_byClass))
  
}

#-------------------------------------------------------------------------------
# Model Testing 

names <- c("Condition", "VSS", "VES", "VCV")

#All tiles
training_2019 <- model_testing(models = MOD, 
                              dataset = condition_training[, ..names], 
                              type = "Training",
                              year = 2019,
                              tile = "All")

testing_2019 <- model_testing(models = MOD, 
                              dataset = condition_testing[, ..names], 
                              type = "Testing",
                              year = 2019,
                              tile = "All")

testing_2018 <- model_testing(models = MOD, 
                              dataset = data_2018[, ..names], 
                              type = "Testing",
                              year = 2018,
                              tile = "All")

testing_2021 <- model_testing(models = MOD, 
                              dataset = data_2021[, ..names], 
                              type = "Testing",
                              year = 2021,
                              tile = "All")

#Spatial
#2019
X0014_Y0024_2019 <- model_testing(models = MOD, 
                                  dataset = condition_testing[tile == "X0014_Y0024", ..names], 
                                  type = "Testing",
                                  year = 2019,
                                  tile = "X0014_Y0024")

X0015_Y0024_2019 <- model_testing(models = MOD, 
                                  dataset = condition_testing[tile == "X0015_Y0024", ..names], 
                                  type = "Testing",
                                  year = 2019,
                                  tile = "X0015_Y0024")

X0016_Y0024_2019 <- model_testing(models = MOD, 
                                  dataset = condition_testing[tile == "X0016_Y0024", ..names], 
                                  type = "Testing",
                                  year = 2019,
                                  tile = "X0016_Y0024")

X0016_Y0025_2019 <- model_testing(models = MOD, 
                                  dataset = condition_testing[tile == "X0016_Y0025", ..names], 
                                  type = "Testing",
                                  year = 2019,
                                  tile = "X0016_Y0025")

X0016_Y0027_2019 <- model_testing(models = MOD, 
                                  dataset = condition_testing[tile == "X0016_Y0027", ..names], 
                                  type = "Testing",
                                  year = 2019,
                                  tile = "X0016_Y0027")

X0017_Y0024_2019 <- model_testing(models = MOD, 
                                  dataset = condition_testing[tile == "X0017_Y0024", ..names], 
                                  type = "Testing",
                                  year = 2019,
                                  tile = "X0017_Y0024")

X0017_Y0026_2019 <- model_testing(models = MOD, 
                                  dataset = condition_testing[tile == "X0017_Y0026", ..names], 
                                  type = "Testing",
                                  year = 2019,
                                  tile = "X0017_Y0026")

X0017_Y0027_2019 <- model_testing(models = MOD, 
                                  dataset = condition_testing[tile == "X0017_Y0027", ..names], 
                                  type = "Testing",
                                  year = 2019,
                                  tile = "X0017_Y0027")

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
                X0017_Y0026_2019$overal,
                X0017_Y0027_2019$overal)

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
                 X0017_Y0026_2019$byClass,
                 X0017_Y0027_2019$byClass)

fwrite(overal, "data/models/overal.csv")
fwrite(byClass, "data/models/byClass.csv")

#-------------------------------------------------------------------------------
# ROC function

get_roc <- function(models = MOD, 
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
  results_cutoff <- data.table()
  
  for(i in 1:n_models) {
    
    #Get model
    model <- models[[i]]
    
    #Predict observations
    predict <- predict(model, dataset, type = 'prob') 
    colnames(predict) <- paste0(colnames(predict), "_pred_", tile)
    
    #Frame 
    true_label <- dummies::dummy(dataset$Condition, sep = ".")
    true_label <- data.frame(true_label)
    colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
    colnames(true_label) <- paste0(colnames(true_label), "_true")
    final <- cbind(true_label, predict)
    
    #Get ROC curve -------------------------------------
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
    plot_roc$repedition <- i
    
    #Collect results
    results <- rbind(results, plot_roc)
    
    #Get cutoff ---------------------------------------
    cutoff_model <- cutoff(final)
    cutoff_model$repedition <- i
    
    #Collect cutoff
    results_cutoff <- rbind(results_cutoff, cutoff_model)
    
    rm(list = c("model", "predict", "true_label", "final",
                "roc_res", "plot_roc", "cutoff_model"))
    
  }
  
  return(list(ROC = results,
              cutoff = results_cutoff))
  
}

#-------------------------------------------------------------------------------
# Get ROCs application

training_2019_roc <- get_roc(models = MOD, 
                             dataset = condition_training[, ..names], 
                             type = "Training",
                             year = 2019,
                             tile = "All")

testing_2019_roc <- get_roc(models = MOD, 
                            dataset = condition_testing[, ..names], 
                            type = "Testing",
                            year = 2019,
                            tile = "All")

testing_2018_roc <- get_roc(models = MOD, 
                            dataset = data_2018[, ..names], 
                            type = "Testing",
                            year = 2018,
                            tile = "All")

testing_2021_roc <- get_roc(models = MOD, 
                            dataset = data_2021[, ..names], 
                            type = "Testing",
                            year = 2021,
                            tile = "All")

#2019
X0014_Y0024_2019_roc <- get_roc(models = MOD, 
                                dataset = condition_testing[tile == "X0014_Y0024", ..names], 
                                type = "Testing",
                                year = 2019,
                                tile = "X0014_Y0024")

X0015_Y0024_2019_roc <- get_roc(models = MOD, 
                                dataset = condition_testing[tile == "X0015_Y0024", ..names], 
                                type = "Testing",
                                year = 2019,
                                tile = "X0015_Y0024")

X0016_Y0024_2019_roc <- get_roc(models = MOD, 
                                dataset = condition_testing[tile == "X0016_Y0024", ..names], 
                                type = "Testing",
                                year = 2019,
                                tile = "X0016_Y0024")

X0016_Y0025_2019_roc <- get_roc(models = MOD, 
                                dataset = condition_testing[tile == "X0016_Y0025", ..names], 
                                type = "Testing",
                                year = 2019,
                                tile = "X0016_Y0025")

X0016_Y0027_2019_roc <- get_roc(models = MOD, 
                                dataset = condition_testing[tile == "X0016_Y0027", ..names], 
                                type = "Testing",
                                year = 2019,
                                tile = "X0016_Y0027")

X0017_Y0024_2019_roc <- get_roc(models = MOD, 
                                dataset = condition_testing[tile == "X0017_Y0024", ..names], 
                                type = "Testing",
                                year = 2019,
                                tile = "X0017_Y0024")

X0017_Y0026_2019_roc <- get_roc(models = MOD, 
                                dataset = condition_testing[tile == "X0017_Y0026", ..names], 
                                type = "Testing",
                                year = 2019,
                                tile = "X0017_Y0026")

X0017_Y0027_2019_roc <- get_roc(models = MOD, 
                                dataset = condition_testing[tile == "X0017_Y0027", ..names], 
                                type = "Testing",
                                year = 2019,
                                tile = "X0017_Y0027")

results_roc <- rbind(training_2019_roc$ROC,
                     testing_2019_roc$ROC,
                     testing_2018_roc$ROC,
                     testing_2021_roc$ROC,
                     X0014_Y0024_2019_roc$ROC,
                     X0015_Y0024_2019_roc$ROC,
                     X0016_Y0024_2019_roc$ROC,
                     X0016_Y0025_2019_roc$ROC,
                     X0016_Y0027_2019_roc$ROC,
                     X0017_Y0024_2019_roc$ROC,
                     X0017_Y0026_2019_roc$ROC,
                     X0017_Y0027_2019_roc$ROC)

fwrite(results_roc, "data/models/rocs.csv")

results_cutoff <- rbind(training_2019_roc$cutoff,
                        testing_2019_roc$cutoff,
                        testing_2018_roc$cutoff,
                        testing_2021_roc$cutoff,
                        X0014_Y0024_2019_roc$cutoff,
                        X0015_Y0024_2019_roc$cutoff,
                        X0016_Y0024_2019_roc$cutoff,
                        X0016_Y0025_2019_roc$cutoff,
                        X0016_Y0027_2019_roc$cutoff,
                        X0017_Y0024_2019_roc$cutoff,
                        X0017_Y0026_2019_roc$cutoff,
                        X0017_Y0027_2019_roc$cutoff)

fwrite(results_cutoff, "data/models/cutoffs.csv")

#-------------------------------------------------------------------------------
# Extraction of the PLSD coefficients (Function)

# ROC function

get_coefficients <- function(models = MOD, ncomp = 3) {
  
  #Number of models
  n_models <- length(models)
  
  #Compile
  healthy <- data.table()
  symtomatic <- data.table()
  dead <- data.table()
  
  for(i in 1:n_models) {
    
    #Get model
    object <- models[[i]]$finalModel
    
    #Get intercept
    coef <- object$coefficients[, , 1:ncomp, drop = FALSE]
    dB <- dim(coef)
    dB[1] <- dB[1] + 1
    dnB <- dimnames(coef)
    dnB[[1]] <- c("(Intercept)", dnB[[1]])
    BInt <- array(dim = dB, dimnames = dnB)
    BInt[-1, , ] <- coef
    
    for (i in seq(along = 1:ncomp)) {
      BInt[1, , i] <- object$Ymeans - object$Xmeans %*% coef[, , i]
    }
    
    coef <- BInt[,,ncomp]
    
    healthy <- rbind(healthy, data.table(Intercept = coef[1,2],
                                         VSS = coef[2,2],
                                         VES = coef[3,2],
                                         VCV = coef[4,2]))
    
    symtomatic <- rbind(symtomatic, data.table(Intercept = coef[1,3],
                                               VSS = coef[2,3],
                                               VES = coef[3,3],
                                               VCV = coef[4,3]))
    
    dead <- rbind(dead, data.table(Intercept = coef[1,1],
                                   VSS = coef[2,1],
                                   VES = coef[3,1],
                                   VCV = coef[4,1]))
    
  }
  
  return(list(healthy = healthy,
              symtomatic = symtomatic,
              dead = dead))
  
}

#Get coefficients
coefficients <- get_coefficients(models = MOD, ncomp = 3)

fwrite(coefficients$healthy, "data/models/coef-healthy.csv")
fwrite(coefficients$symtomatic, "data/models/coef-symtomatic.csv")
fwrite(coefficients$dead, "data/models/coef-dead.csv")
