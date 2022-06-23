################################################################################
##### 05 - Model (training - testing)
################################################################################

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(caret)
library(ggplot2)
library(mlbench)
library(plotROC)

#-------------------------------------------------------------------------------
# Reading and cleaning

#Reading -------------

X0014_Y0024 <- fread("F:/FORCE/model/data/X0014_0024_dVI.txt")
X0015_Y0024 <- fread("F:/FORCE/model/data/X0015_0024_dVI.txt")
#X0016_Y0024 <- fread("F:/FORCE/model/data/X0016_0024_dVI.txt")

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
frame <- data[, row[which.min(dCCI)], by = c("ID", "tile")]
colnames(frame)[3] <- "row"
frame <- na.exclude(frame)
data <- data[frame$row, ]
condition <- is.na(data$condition)
data <- subset(data, condition != "")
data <- data[, c(12, 1:11)]

#Data should be clean and it is ready to export

#Export for training
fwrite(data, "F:/FORCE/model/data/data_clean.csv")

#-------------------------------------------------------------------------------
# Model preparation

#Read
data <- fread("F:/FORCE/model/data/data_clean.csv")

#Data split
data[condition == "wilted", factor := "wilted", ]
data[condition != "wilted", factor := "non_wilted", ]

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
fwrite(training, "F:/FORCE/model/training/training.csv")
fwrite(testing, "F:/FORCE/model/training/testing.csv")

#-------------------------------------------------------------------------------
# Model Training

#Repeated 10-fold cross-validation
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           savePredictions = TRUE)

data_training <- training[, c(13, 9:11)]
weights <- training$weight

#Model train
set.seed(825)
bayesglm_model <- train(factor ~ ., data = data_training, 
                        method = 'bayesglm', 
                        weights = weights,
                        trControl = fitControl,
                        metric = "ROC")

#-------------------------------------------------------------------------------
# Model Testing

data_testing <- testing[, c(13, 9:11)]
weights <- training$weight


training_matrix <- data.frame(observed = as.factor(training$factor),
                               predicted = predict(bayesglm_model))

testing_matrix <- data.frame(observed = as.factor(testing$factor),
                              predicted = predict(bayesglm_model, newdata = testing))

training_cm <- caret::confusionMatrix(training_matrix$observed, training_matrix$predicted)
testing_cm <- caret::confusionMatrix(testing_matrix$observed, testing_matrix$predicted)

ggplot(bayesglm_model$pred, aes(m=wilted, d=factor(obs, levels = c("non_wilted", "wilted")))) + 
  geom_roc() + 
  coord_equal() +
  style_roc()



