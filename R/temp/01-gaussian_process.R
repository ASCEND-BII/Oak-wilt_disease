################################################################################
#' @title Gaussian Process for training the algorithm
################################################################################

#-------------------------------------------------------------------------------
# Library
library(data.table)
library(caret)
library(doParallel)

#-------------------------------------------------------------------------------
# Arguments 
#' @param synt_spectra The .csv with the synthetic mixing spectra for a endmember

#-------------------------------------------------------------------------------
# Data processing

#-------------------------------------------------------------------------------
# Get sigma for training
sigma_optimal <- function(data, iterations = 100, threads = 28) {
  
  #To fill
  complete_frame <- data.table()
  
  #Model control
  control <- trainControl(method = "cv", 
                          number = 5, 
                          search = "grid", 
                          allowParallel = TRUE, 
                          verboseIter = FALSE)
  
  #Progress bar
  pb <- txtProgressBar(min = 1, max = iterations, style = 3)
  
  #Loop over iterations
  for(i in 1:iterations) {
    
    #Progress bar
    setTxtProgressBar(pb, i)
    
    #Random sample
    data_sample <- data[sample(c(1:nrow(data)), floor(nrow(data)*0.5), replace = FALSE), ]
    
    #Cluster
    cl <- makePSOCKcluster(threads)
    registerDoParallel(cl)
    
    #Model
    model <- train(SAM ~ ., 
                   data = data_sample, 
                   method = 'gaussprRadial',
                   trControl = control,
                   #preProcess = c("center", "scale"),
                   tuneGrid = expand.grid(sigma = seq(0.01, 1.0, by = 0.01)),
                   allowParallel = TRUE)

    #Stop cluster
    stopCluster(cl)
    gc()
    
    results <- as.data.table(model$results)
    results <- cbind(iteration = i,
                     results,
                     best_sigma = model$bestTune)
    
    
    complete_frame <- rbind(complete_frame, results)
    
  }
  
  return(complete_frame)
  
}


#Run function to find sigma
#Healthy
synt_spectra <- fread("data/healty_ssam.csv")
results <- sigma_optimal(synt_spectra, iterations = 5)
fwrite(results, "data/healty_sigma.csv")

#OW
synt_spectra <- fread("data/wilted_ssam.csv")
results <- sigma_optimal(synt_spectra, iterations = 5)
fwrite(results, "data/wilted_sigma.csv")

#Dead
synt_spectra <- fread("data/dead_ssam.csv")
results <- sigma_optimal(synt_spectra, iterations = 5)
fwrite(results, "data/dead_sigma.csv")


################################################################################
### Formal algorithm training
run_GPR <- function(data, sigma, threads = 30) {
  
  #Elements
  threads <- 28
  control <- trainControl(method = "repeatedcv", 
                          number = 10, 
                          repeats = 10,
                          search = "grid",
                          allowParallel = TRUE, 
                          verboseIter = TRUE)
  
  #Model--------------------------------
  cl <- makePSOCKcluster(threads)
  registerDoParallel(cl)
  
  model <- train(SAM ~ ., 
                 data = data, 
                 method = 'gaussprRadial',
                 trControl = control,
                 #preProcess = c("center","scale"),
                 tuneGrid = expand.grid(sigma = sigma),
                 allowParallel = TRUE)
  
  stopCluster(cl)
  gc()
  
  return(model)
  
}

#Healthy
synt_spectra <- fread("data/healty_ssam.csv")
synt_spectra[, NDBI := (SWIR1 - NIR) / (SWIR1 + NIR)]
synt_spectra[, CIre := (REDEDGE3 / REDEDGE1) - 1]
synt_spectra <- synt_spectra[, c(1, 6, 7)]
model <- run_GPR(synt_spectra, sigma = 0.6, threads = 30)
saveRDS(model, "data/healty_model.rds")

#OW
synt_spectra <- fread("data/wilted_ssam.csv")
synt_spectra[, NDBI := (SWIR1 - NIR) / (SWIR1 + NIR)]
synt_spectra[, CIre := (REDEDGE3 / REDEDGE1) - 1]
synt_spectra <- synt_spectra[, c(1, 6, 7)]
model <- run_GPR(synt_spectra, sigma = 0.6, threads = 30)
saveRDS(model, "data/wilted_model.rds")

#Dead
synt_spectra <- fread("data/dead_ssam.csv")
synt_spectra[, NDBI := (SWIR1 - NIR) / (SWIR1 + NIR)]
synt_spectra[, CIre := (REDEDGE3 / REDEDGE1) - 1]
synt_spectra <- synt_spectra[, c(1, 6, 7)]
model <- run_GPR(synt_spectra, sigma = 0.6, threads = 30)
saveRDS(model, "data/dead_model.rds")





