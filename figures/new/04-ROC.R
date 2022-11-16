################################################################################
##### 03 - Plot of the classification assesement
################################################################################

#' @description A script for ploting the comparison of polar metrics.

#-------------------------------------------------------------------------------
#' Libraries

library(data.table)
library(ggplot2)
library(ggpubr)
library(multiROC)


#Layout properties -------------------------------------------------------------

#pa <- c("#1b9e77", "#d95f02", "#7570b3")
pa <- c("#1b9e77", "#d95f02", "#7570b3", "#FF0000", "#2c7fb8")
tamano <- 14
tamano2 <- 12

th <- theme_bw(base_size = tamano) + theme(plot.background = element_blank(),
                                           panel.grid.major = element_blank(),
                                           panel.grid.minor = element_blank(),
                                           plot.margin = margin(5, 6, 5, 5, "pt"),
                                           axis.text.x = element_text(color = "black", size = tamano2),
                                           axis.text.y = element_text(color = "black", size = tamano2),
                                           strip.text.x = element_text(size = tamano, color = "black"),
                                           strip.text.y = element_text(size = tamano, color = "black"),
                                           strip.background = element_rect(color= "black", fill="grey90", linetype="solid"))

#-------------------------------------------------------------------------------
# Root path

path <- "F:/TRAINING/level3_lsf-pixels"
path <- "/media/antonio/antonio_ssd/TRAINING/level3_lsf-pixels"

#Plot --------------------------------------------------------------------------

# Get model of interest
models <- readRDS("data/models/models.rds")
SVM <- models$SVM

# Get testing datasets
training_2019 <- fread(paste0(path, "/training_2019.csv"))
testing_2019 <- fread(paste0(path, "/testing_2019.csv"))
data <- fread(paste0(path, "/master_training.csv"))
testing_2018 <- subset(data, year == 2018)
testing_2021 <- subset(data, year == 2021)

#Names for training
names <-  c("Condition", "PPM", "VGM", "VCV", "IFR")

#Predict observations
#2018
predict_2018 <- predict(SVM, testing_2018[, ..names], type = 'prob') 
colnames(predict_2018) <- paste(colnames(predict_2019), "_pred_2018")

true_label_2018 <- dummies::dummy(testing_2018$Condition, sep = ".")
true_label_2018 <- data.frame(true_label_2018)
colnames(true_label_2018) <- gsub(".*?\\.", "", colnames(true_label_2018))
colnames(true_label_2018) <- paste(colnames(true_label_2018), "_true")
final_2018 <- cbind(true_label_2018, predict_2018)

roc_res_2018 <- multi_roc(final_2018, force_diag=T)
plot_roc_2018 <- plot_roc_data(roc_res_2018)
colnames(plot_roc_2018)[3] <- "Condition"
plot_roc_2018$Condition <- factor(plot_roc_2018$Condition, levels = c("Healthy ", "Wilted ", "Dead ", "Macro", "Micro"))

plot_2018 <- ggplot(plot_roc_2018, aes(x = 1-Specificity, y=Sensitivity)) +
  geom_path(aes(color = Condition, linetype = Condition), size= 0.6) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotted') +
  scale_color_manual(values = pa) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "dotted", "dotted")) +
  th + 
  scale_x_continuous(limits = c(-0.01, 1.01), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.01, 1.05), expand = c(0, 0))

#2019
predict_2019 <- predict(SVM, testing_2019[, ..names], type = 'prob') 
colnames(predict_2019) <- paste(colnames(predict_2019), "_pred_2019")

true_label_2019 <- dummies::dummy(testing_2019$Condition, sep = ".")
true_label_2019 <- data.frame(true_label_2019)
colnames(true_label_2019) <- gsub(".*?\\.", "", colnames(true_label_2019))
colnames(true_label_2019) <- paste(colnames(true_label_2019), "_true")
final_2019 <- cbind(true_label_2019, predict_2019)

roc_res_2019 <- multi_roc(final_2019, force_diag=T)
plot_roc_2019 <- plot_roc_data(roc_res_2019)
colnames(plot_roc_2019)[3] <- "Condition"
plot_roc_2019$Condition <- factor(plot_roc_2019$Condition, levels = c("Healthy ", "Wilted ", "Dead ", "Macro", "Micro"))

plot_2019 <- ggplot(plot_roc_2019, aes(x = 1-Specificity, y=Sensitivity)) +
  geom_path(aes(color = Condition, linetype = Condition), size= 0.6) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotted') +
  scale_color_manual(values = pa) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "dotted", "dotted")) +
  th + 
  scale_x_continuous(limits = c(-0.01, 1.01), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.01, 1.05), expand = c(0, 0))

#2021
predict_2021 <- predict(SVM, testing_2021[, ..names], type = 'prob') 
colnames(predict_2021) <- paste(colnames(predict_2019), "_pred_2021")

true_label_2021 <- dummies::dummy(testing_2021$Condition, sep = ".")
true_label_2021 <- data.frame(true_label_2021)
colnames(true_label_2021) <- gsub(".*?\\.", "", colnames(true_label_2021))
colnames(true_label_2021) <- paste(colnames(true_label_2021), "_true")
final_2021 <- cbind(true_label_2021, predict_2021)

roc_res_2021 <- multi_roc(final_2021, force_diag=T)
plot_roc_2021 <- plot_roc_data(roc_res_2021)
colnames(plot_roc_2021)[3] <- "Condition"
plot_roc_2021$Condition <- factor(plot_roc_2021$Condition, levels = c("Healthy ", "Wilted ", "Dead ", "Macro", "Micro"))

plot_2021 <- ggplot(plot_roc_2021, aes(x = 1-Specificity, y=Sensitivity)) +
  geom_path(aes(color = Condition, linetype = Condition), size= 0.6) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotted') +
  scale_color_manual(values = pa) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "dotted", "dotted")) +
  th + 
  scale_x_continuous(limits = c(-0.01, 1.01), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.01, 1.05), expand = c(0, 0))
