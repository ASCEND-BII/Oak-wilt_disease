################################################################################
##### 05 - Model (training - testing)
################################################################################

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(caret)
library(ggplot2)
library(viridis)
library(mlbench)
library(plotROC)
library(terra)

#-------------------------------------------------------------------------------
# Root path

path <- "F:/FORCE"

#-------------------------------------------------------------------------------
# Reading and cleaning

#Reading -------------

X0014_Y0024 <- fread(paste0(path, "/model/data/X0014_0024_dVI.txt"))
X0015_Y0024 <- fread(paste0(path, "/model/data/X0015_0024_dVI.txt"))
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
data$row <- 1:nrow(data)

#Select minimum dCCI
frame <- data[, row[which.max(weight)], by = c("ID", "tile")]
colnames(frame)[3] <- "row"
frame <- na.exclude(frame)
data <- data[frame$row, ]
condition <- is.na(data$condition)
data <- subset(data, condition != "")
data <- data[, c(12, 1:11)]

#Look for outlayers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

data[, dCCI := remove_outliers(dCCI), by = condition]
data[, dCCI := remove_outliers(dCCI), by = condition]
data[, dNDW := remove_outliers(dNDW), by = condition]
data[, dCRE := remove_outliers(dCRE), by = condition]
data[, kND := remove_outliers(kND), by = condition]

ggplot(data, aes(x= condition, y= kND, fill = condition)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)

#Remove NAs
data <- na.exclude(data)

#Data should be clean and it is ready to export

#Export for training
fwrite(data, paste0(path, "/model/data/data_clean.csv"))

#-------------------------------------------------------------------------------
# Model preparation

#Read
data <- fread(paste0(path, "/model/data/data_clean.csv"))

#Data split
data[condition == "wilted", factor := "wilted", ]
data[condition != "wilted", factor := "non_wilted", ]

#Proportions
table(data$factor)

#Separate
wilted <- subset(data, factor == "wilted")
no_wilted <- subset(data, factor == "non_wilted")

#Index for training
ind_wilted <- createDataPartition(wilted$tile,
                                  p = 0.5,
                                  list = FALSE,
                                  times = 1)

ind_no_wilted <- createDataPartition(no_wilted$tile,
                                     p = 0.75,
                                     list = FALSE,
                                     times = 1)

#Get training and testing
training <- rbind(wilted[ind_wilted], no_wilted[ind_no_wilted])
testing <- rbind(wilted[!ind_wilted], no_wilted[!ind_no_wilted])

#Data should be ready for the models and to export it
fwrite(training, paste0(path, "/model/training/training.csv"))
fwrite(testing, paste0(path, "/model/training/testing.csv"))

#-------------------------------------------------------------------------------
# Model Training

#Repeated 10-fold cross-validation
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 1,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           savePredictions = TRUE)

data_training <- training[, c(13, 9:12)]
weights <- training$area

#Model train
set.seed(825)
bayesglm_model <- train(factor ~ ., data = data_training, 
                        method = 'bayesglm', 
                        trControl = fitControl,
                        metric = "ROC")

bayesglm_model <- train(factor ~ ., data = data_training, 
                        method = 'rf', 
                        weights = weights,
                        trControl = fitControl,
                        metric = "ROC")


fitControl <- trainControl(method = "none",
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           savePredictions = TRUE)

fitControl <- trainControl(method = "cv",
                           number = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           savePredictions = TRUE)

nnetGrid <-  expand.grid(size = seq(from = 1, to = 10, by = 1),
                         decay = seq(from = 0.1, to = 0.5, by = 0.1))

bayesglm_model <- train(factor ~ ., data = data_training, 
                        method = 'nnet', 
                        weights = weights,
                        trControl = fitControl,
                        metric = "ROC",
                        tuneGrid = nnetGrid,
                        allowParallel=TRUE)



library(doParallel)
no_cores <- detectCores() - 4
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)

bayesglm_model <- train(factor ~ ., data = data_training, 
                        method = 'gaussprLinear', 
                        weights = weights,
                        trControl = fitControl,
                        metric = "ROC",
                        allowParallel=TRUE)

stopCluster(cl)
registerDoSEQ()

#-------------------------------------------------------------------------------
# Model Testing

data_testing <- testing[, c(13, 9:12)]

training_matrix <- data.frame(observed = as.factor(training$factor),
                               predicted = predict(bayesglm_model))

testing_matrix <- data.frame(observed = as.factor(testing$factor),
                              predicted = predict(bayesglm_model, newdata = testing))

training_cm <- caret::confusionMatrix(training_matrix$observed, training_matrix$predicted)
testing_cm <- caret::confusionMatrix(testing_matrix$observed, testing_matrix$predicted)

ggplot(bayesglm_model$pred, aes(m=wilted, d=factor(obs, levels = c("wilted", "non_wilted")))) + 
  geom_roc() + 
  coord_equal() +
  style_roc()

prob <- predict(bayesglm_model, type = "prob")[, 2]
plot(prob ~ data_training$dCCI)
plot(prob ~ data_training$dNDW)
plot(prob ~ data_training$dCRE)
plot(prob ~ data_training$kND)

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


avNNet(x, ...)