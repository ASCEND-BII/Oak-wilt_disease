library(shapes)
library(data.table)
library(rgl)

#-------------------------------------------------------------------------------
# Root path

path <- "/media/antonio/antonio_ssd/FORCE"
path <- "E:/FORCE"

#-------------------------------------------------------------------------------
# Reading and cleaning

#Reading -------------
X0014_Y0024 <- fread(paste0(path, "/level3_shifted/X0014_0024_VI_clean.txt"))
X0015_Y0024 <- fread(paste0(path, "/level3_shifted/X0015_0024_VI_clean.txt"))

#Add tiles
X0014_Y0024$tile <- "X0014_Y0024"
X0015_Y0024$tile <- "X0015_Y0024"
#X0016_Y0024$tile <- "X0016_Y0024"

#rbind tiles
data <- rbind(X0014_Y0024, X0015_Y0024)
#Procrustes

frame <- subset(data, year(as.Date(date)) == 2019)
info <- frame[, c(1, 2, 12)]
frame <- frame[, c(12, 1, 2, 3, 8, 9, 11)]
frame <- subset(frame, month(as.Date(date)) > 2)
frame <- subset(frame, month(as.Date(date)) < 11)

frame$CCI <- tanh((frame$CCI/10000)^2)*10
frame$CRE <- frame$CRE/1000
frame$NDW <- tanh((frame$NDW/10000)^2)*10

a <- frame[ID == 1 & tile == "X0014_Y0024"]
plot3d(a[, 5:6])
plot(a$date, tanh((a$NDW/10000)^2))
plot(a$date, (a$NDW/10000))
plot(a$date, (a$CRE/1000))

#Unique combination
remove <- na.exclude(frame)
unique_IDs <- remove[, .N, by= c("tile", "ID", "condition")]
unique_IDs <- unique_IDs[N == 15]

#k x m x n real array, (or k x n complex matrix for m=2 is OK), where k is the number of points, m is the number of dimensions, and n is the sample size. 

to_transform <- array(NA, dim=c(length(unique(frame$date)), 3, nrow(unique_IDs)))

for(n in 1:nrow(unique_IDs)) {
  
  sub_frame <- subset(frame, tile == unique_IDs$tile[n] & ID == unique_IDs$ID[n])
  
  to_transform[, 1, n] <- sub_frame$CCI
  to_transform[, 2, n] <- sub_frame$CRE
  to_transform[, 3, n] <- sub_frame$NDW
  
} 

dim(to_transform)

GPT <- procGPA(to_transform, 
               scale = TRUE, 
               reflect = FALSE, 
               eigen2d = FALSE, 
               tol1 = 1e-05, 
               tol2 = 1e-05, 
               tangentcoords = "residual", 
               proc.output = TRUE, 
               distances = TRUE, 
               pcaoutput = TRUE, 
               alpha= 0, 
               affine=FALSE)

shapepca(GPT, type="g")

plot(GPT$rawscores[,1], GPT$rawscores[,2],xlab="PC1",ylab="PC2")

par(mfrow=c(2,2))
plot(GPT$rawscores[,1], GPT$rawscores[,2],xlab="PC1",ylab="PC2")
title("PC scores")
plot(GPT$rawscores[,2], GPT$rawscores[,3],xlab="PC2",ylab="PC3")
plot(GPT$rawscores[,1], GPT$rawscores[,3],xlab="PC1",ylab="PC3")
plot(GPT$size, GPT$rho,xlab="size",ylab="rho")
title("Size versus shape distance")

round(GPT$percent, 2)

scores <- as.data.table(GPT$rawscores)
scores <- cbind(unique_IDs, scores)

#Create plots
plot <- ggplot(scores, aes(x = condition, y = PC1, fill = condition)) +
  geom_point(shape = 21, size= 1, position = position_jitterdodge(), color="white", alpha= 0.8) +
  geom_violin(alpha=0.4, position = position_dodge(width = .75), size= 0.5, color="black") +
  geom_boxplot(width=0.1, color = "white", alpha = .30, outlier.shape = NA) 
  
#-------------------------------------------------------------------------------
# Model preparation

#Read
data <- fread(paste0(path, "/model/data/data_clean.csv"))
data <- scores

#Data split
data[condition == "wilted", factor_wilted := "wilted", ]
data[condition != "wilted", factor_wilted := "non_wilted", ]

data[condition == "healthy", factor_healthy := "healthy", ]
data[condition != "healthy", factor_healthy := "non_healthy", ]

data[condition == "dead", factor_dead := "dead", ]
data[condition != "dead", factor_dead := "non_dead", ]

#Data partitions ---------------------------------------------------------------
#Wilted ---------------------------
table(data$factor_wilted) #629 508 

#Separate
wilted <- subset(data, factor_wilted == "wilted")
no_wilted <- subset(data, factor_wilted == "non_wilted")

#Split
ind_wilted <- createDataPartition(wilted$tile,
                                  p = 0.5996759,
                                  list = FALSE,
                                  times = 1)

ind_no_wilted <- createDataPartition(no_wilted$tile,
                                     p = 0.5417277,
                                     list = FALSE,
                                     times = 1)


#Healthy ---------------------------
table(data$factor_healthy) #358 779

#Separate
healthy <- subset(data, factor_healthy == "healthy")
no_healthy <- subset(data, factor_healthy == "non_healthy")

#Split
ind_healthy <- createDataPartition(healthy$tile,
                                   p = 0.6,
                                   list = FALSE,
                                   times = 1)

ind_no_healthy <- createDataPartition(no_healthy$tile,
                                      p = 0.39,
                                      list = FALSE,
                                      times = 1)

#Dead ---------------------------
table(data$factor_dead) #271 866

#Separate
dead <- subset(data, factor_dead == "dead")
no_dead <- subset(data, factor_dead == "non_dead")

ind_dead <- createDataPartition(dead$tile,
                                p = 0.6004289,
                                list = FALSE,
                                times = 1)

ind_no_dead <- createDataPartition(no_dead$tile,
                                   p = 0.1990993,
                                   list = FALSE,
                                   times = 1)

#Get training and testing export -----------------------------------------------
#wilted
wilted_training <- rbind(wilted[ind_wilted], no_wilted[ind_no_wilted])
wilted_testing <- rbind(wilted[!ind_wilted], no_wilted[!ind_no_wilted])

fwrite(wilted_training, paste0(path, "/model/training/wilted_training.csv"))
fwrite(wilted_testing, paste0(path, "/model/training/wilted_testing.csv"))

#Healthy
healthy_training <- rbind(healthy[ind_healthy], no_healthy[ind_no_healthy])
healthy_testing <- rbind(healthy[!ind_healthy], no_healthy[!ind_no_healthy])

fwrite(healthy_training, paste0(path, "/model/training/healthy_training.csv"))
fwrite(healthy_testing, paste0(path, "/model/training/healthy_testing.csv"))

#Dead
dead_training <- rbind(dead[ind_dead], no_dead[ind_no_dead])
dead_testing <- rbind(dead[!ind_dead], no_dead[!ind_no_dead])

fwrite(dead_training, paste0(path, "/model/training/dead_training.csv"))
fwrite(dead_testing, paste0(path, "/model/training/dead_testing.csv"))

#-------------------------------------------------------------------------------
# Model Training

#Repeated 10-fold cross-validation
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           savePredictions = TRUE)

#Select columns of interest
wilted_weights <- wilted_training$weight
wilted_training <- wilted_training[, c(13, 8:11)]
healthy_weights <- healthy_training$weight
healthy_training <- healthy_training[, c(14, 8:11)]
dead_weights <- dead_training$weight
dead_training <- dead_training[, c(15, 8:11)]


#Model
model_training <- function(wilted_training, healthy_training, dead_training) {
  
  set.seed(825)
  wilted_model <- train(factor_wilted ~ ., data = wilted_training[, c(5:49, 50)], 
                        method = 'bayesglm', 
                        trControl = fitControl,
                        #weights = wilted_weights*100,
                        metric = "ROC")
  
  set.seed(825)
  healthy_model <- train(factor_healthy ~ ., data = healthy_training[, c(5:49, 51)], 
                         method = 'bayesglm', 
                         trControl = fitControl,
                         #weights = healthy_weights*100,
                         metric = "ROC")
  
  set.seed(825)
  dead_model <- train(factor_dead ~ ., data = dead_training[, c(5:49, 52)], 
                      method = 'bayesglm', 
                      trControl = fitControl,
                      #weights = dead_weights*100,
                      metric = "ROC")
  
  return(list(wilted = wilted_model,
              healthy = healthy_model,
              dead = dead_model))
  
}

models <- model_training(wilted_training, healthy_training, dead_training)

#Export model
saveRDS(models, "data/models/models.rds")

#-------------------------------------------------------------------------------
# Model Testing

#Select columns of interest
wilted_testing <- wilted_testing[, c(50, 5:49)]
healthy_testing <- healthy_testing[, c(14, 8:11)]
dead_testing <- dead_testing[, c(15, 8:11)]

#Model testing

model_testing <- function(models, 
                          wilted_training, wilted_testing, 
                          healthy_training, healthy_testing, 
                          dead_training, dead_testing) {
  
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
  
  frame <- rbind(wilted_training, wilted_testing,
                 healthy_training, healthy_testing,
                 dead_training, dead_testing)
  
  colnames(frame)[3:9] <- names(dead_training_cm$overall)
  colnames(frame)[10:20] <- names(dead_training_cm$byClass)
  
  return(frame)
  
}

validation <- model_testing(models, 
                            wilted_training, wilted_testing, 
                            healthy_training, healthy_testing, 
                            dead_training, dead_testing)




