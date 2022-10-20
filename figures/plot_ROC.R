cop <- rast("/media/antonio/antonio_ssd/FORCE/corregistration/X0014_Y0024/X0014_Y0024_red.tif")


files <- list.files(path = "/media/antonio/Work/Oak-wilt/NAIP", 
                    pattern = ".tif", 
                    all.files = TRUE,
                    full.names = FALSE, 
                    recursive = TRUE)

#Arrange path in frame
frame <- data.table(matrix(unlist(strsplit(files, "/")), 
                           nrow= length(files), 
                           byrow=TRUE), stringsAsFactors=FALSE)
colnames(frame) <- c("folder", "file")

for(i in 1:nrow(frame)) {
  
  naip <- rast(paste0("/media/antonio/Work/Oak-wilt/NAIP/", frame$folder[i], "/", frame$file[i]))
  naip <- project(naip, cop, method = "cubic")
  
  writeRaster(naip, 
              paste0("/media/antonio/Work/Oak-wilt/NAIP_projected/", frame$file[i]),
              overwrite=TRUE)
              
  
}




library(ggplot2)
library(plotROC)
library(pROC)

a <- models$condition
prediction <-  predict(a, newdata= condition_testing, type="prob")

a$pred$healthy

b <- multiclass.roc(condition_testing$condition, prediction)


ggplot(rfFit$pred[selectedIndices, ], 
       aes(m = M, d = factor(obs, levels = c("R", "M")))) + 
  geom_roc(hjust = -0.4, vjust = 1.5) + coord_equal()



library(caret)
library(ggplot2)
library(mlbench)
library(plotROC)

data(Sonar)

ctrl <- trainControl(method="cv", summaryFunction=twoClassSummary, classProbs=T,
                     savePredictions = T)

rfFit <- train(Class ~ ., data=Sonar, method="rf", preProc=c("center", "scale"), 
               trControl=ctrl)

# Select a parameter setting
selectedIndices <- rfFit$pred$mtry == 2



HD <- data.table(condition
  
)


require(multiROC)
data(iris)
head(iris)

require(multiROC)
data(iris)
head(iris)

set.seed(123456)
total_number <- nrow(iris)
train_idx <- sample(total_number, round(total_number*0.6))
train_df <- iris[train_idx, ]
test_df <- iris[-train_idx, ]

iris <- condition_testing
rf_pred <- predict(a, iris, type = 'prob') 
rf_pred <- data.frame(rf_pred)
colnames(rf_pred) <- paste(colnames(rf_pred), "_pred")

true_label <- dummies::dummy(iris$condition, sep = ".")
true_label <- data.frame(true_label)
colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
colnames(true_label) <- paste(colnames(true_label), "_true")
final_df <- cbind(true_label, rf_pred)

roc_res <- multi_roc(final_df)
plot_roc_df <- plot_roc_data(roc_res)
plot_roc_df <- as.data.table(plot_roc_df)
plot_roc_df[Group == "dead ", Group := "Dead"]
plot_roc_df[Group == "wilted ", Group := "Wilted"]
plot_roc_df[Group == "healthy ", Group := "Healthy"]
plot_roc_df$Group <- factor(plot_roc_df$Group, levels = c("Healthy", "Wilted", "Dead", "Macro", "Micro"))
colnames(plot_roc_df)[3] <- "Condition"

require(ggplot2)
plot <- ggplot(plot_roc_df, aes(x = 1-Specificity, y=Sensitivity)) +
  geom_path(aes(color = Condition), size= 1) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotted') +
  scale_color_manual(values = c("#228b22", "#FF0000", "grey40", "#fec44f", "#2c7fb8")) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "dotted", "dotted")) +
  theme_bw() 

jpeg("figures/Figure_ROC.jpeg", width = 16, height = 10, units = "cm", res = 600, pointsize = 14)

plot

dev.off()  










