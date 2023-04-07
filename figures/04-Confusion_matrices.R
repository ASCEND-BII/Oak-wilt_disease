################################################################################
##### 04 - Plot confusion matrices
################################################################################

#' @description A script for extracting important metrics of performance and 
#' coefficients in the final selected model.

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(caret)
library(cvms)
library(ggplot2)
library(ggpubr)

#-------------------------------------------------------------------------------
# Root path

path <- "/media/antonio/Work/Projects/Oak-wilt_mapping/level3_pixel-extraction"
path <- "E:/Projects/Oak-wilt_mapping/level3_pixel-extraction"

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
# Plot confusion function
plot_confusion <- function(models, 
                           dataset,
                           type = "Training",
                           year = 2019,
                           tile = "All") {
  
  #Number of models
  n_models <- length(models)
  
  #To future col names
  cm <- NA
  
  #Compile
  observed <- as.factor(dataset$Condition)
  pred <- data.table()
  
  for(i in 1:n_models) {
    
    PLSD <- models[[i]]
    
    #Condition
    predicted <- predict(PLSD, newdata = dataset[, c(2:4)])
    pred <- cbind(pred, predicted)
    colnames(pred)[i] <- paste0("model_", i)
    
  }
  
  predicted <- character()
  fact_names <- c("Healthy", "Symptomatic", "Dead")
  
  for(i in 1:nrow(pred)) {
    
    sub_pred <- as.vector(as.matrix(pred[i,]))
    
    healthy <- length(sub_pred[sub_pred == "Healthy"])
    symptomatic <- length(sub_pred[sub_pred == "Symptomatic"])
    dead <- length(sub_pred[sub_pred == "Dead"])
    
    vector <- c(healthy, symptomatic, dead)
    predicted[i] <- fact_names[which.max(vector)][1]
    
  }
  
  predicted <- as.factor(predicted)
  predicted <- factor(predicted, levels = c("Healthy", "Symptomatic", "Dead"))
  observed <- factor(observed, levels = c("Healthy", "Symptomatic", "Dead"))
  
  cm <- data.frame(Observed = as.character(observed),
                   Predicted = as.character(predicted))
  eval <- evaluate(data = cm,
                   target_col = "Observed",
                   prediction_cols = "Predicted",
                   type = "multinomial")
  
  cm <- eval[["Confusion Matrix"]][[1]]
  colnames(cm) <- c("Predicted", "Observed", "N")
  
  plot <- plot_confusion_matrix(cm,
                                class_order = c("Dead", "Symptomatic", "Healthy"),
                                target_col = "Observed",
                                prediction_col = "Predicted",
                                add_row_percentages = FALSE,
                                add_col_percentages = FALSE)
  
  return(plot)
  
}

#-------------------------------------------------------------------------------
# Figures


names <- c("Condition", "VSS", "VES", "VCV")

#All tiles
training_2019 <- plot_confusion(models = MOD, 
                                dataset = condition_training[, ..names], 
                                type = "Training",
                                year = 2019,
                                tile = "All")

testing_2019 <- plot_confusion(models = MOD, 
                               dataset = condition_testing[, ..names], 
                               type = "Testing",
                               year = 2019,
                               tile = "All")

testing_2018 <- plot_confusion(models = MOD, 
                               dataset = data_2018[, ..names], 
                               type = "Testing",
                               year = 2018,
                               tile = "All")

testing_2021 <- plot_confusion(models = MOD, 
                               dataset = data_2021[, ..names], 
                               type = "Testing",
                               year = 2021,
                               tile = "All")

#Spatial
#2019
X0014_Y0024_2019 <- plot_confusion(models = MOD, 
                                   dataset = condition_testing[tile == "X0014_Y0024", ..names], 
                                   type = "Testing",
                                   year = 2019,
                                   tile = "X0014_Y0024")

X0015_Y0024_2019 <- plot_confusion(models = MOD, 
                                   dataset = condition_testing[tile == "X0015_Y0024", ..names], 
                                   type = "Testing",
                                   year = 2019,
                                   tile = "X0015_Y0024")

X0016_Y0024_2019 <- plot_confusion(models = MOD, 
                                   dataset = condition_testing[tile == "X0016_Y0024", ..names], 
                                   type = "Testing",
                                   year = 2019,
                                   tile = "X0016_Y0024")

X0016_Y0025_2019 <- plot_confusion(models = MOD, 
                                   dataset = condition_testing[tile == "X0016_Y0025", ..names], 
                                   type = "Testing",
                                   year = 2019,
                                   tile = "X0016_Y0025")

X0016_Y0027_2019 <- plot_confusion(models = MOD, 
                                   dataset = condition_testing[tile == "X0016_Y0027", ..names], 
                                   type = "Testing",
                                   year = 2019,
                                   tile = "X0016_Y0027")

X0017_Y0024_2019 <- plot_confusion(models = MOD, 
                                   dataset = condition_testing[tile == "X0017_Y0024", ..names], 
                                   type = "Testing",
                                   year = 2019,
                                   tile = "X0017_Y0024")

X0017_Y0026_2019 <- plot_confusion(models = MOD, 
                                   dataset = condition_testing[tile == "X0017_Y0026", ..names], 
                                   type = "Testing",
                                   year = 2019,
                                   tile = "X0017_Y0026")

X0017_Y0027_2019 <- plot_confusion(models = MOD, 
                                   dataset = condition_testing[tile == "X0017_Y0027", ..names], 
                                   type = "Testing",
                                   year = 2019,
                                   tile = "X0017_Y0027")


#Unite figure -----------------------------------------------------------------
#figure
figure_temporal <- ggarrange(testing_2018, 
                             testing_2019, 
                             testing_2021,
                             labels = c("2018", "2019", "2021"),
                             font.label = list(size = 15, color = "black", face = "plain", family = NULL),
                             label.x = 0.05,
                             label.y = 0.97,
                             ncol = 2, nrow = 2,  align = "hv",
                             widths = c(2, 2),
                             heights = c(2, 2),
                             common.legend = TRUE)



jpeg("figures/cm_temporal.jpeg", quality = 100, res = 300, width = 180, height = 210, units = "mm", pointsize = 11) # JPEG device

figure_temporal

dev.off()


figure_spatial <- ggarrange(X0014_Y0024_2019, X0015_Y0024_2019,
                            X0016_Y0024_2019, X0016_Y0025_2019,
                            X0016_Y0027_2019, X0017_Y0024_2019,
                            X0017_Y0026_2019, X0017_Y0027_2019,
                            labels = c("X0014_Y0024", "X0015_Y0024",
                                       "X0016_Y0024", "X0016_Y0025",
                                       "X0016_Y0027", "X0017_Y0024",
                                       "X0017_Y0026", "X0017_Y0027"),
                            font.label = list(size = 12, color = "black", face = "plain", family = NULL),
                            label.x = 0.05,
                            label.y = 0.97,
                            ncol = 2, nrow = 4,  align = "hv",
                            widths = c(2, 2),
                            heights = c(2, 2),
                            common.legend = TRUE)



jpeg("figures/cm_spatial.jpeg", quality = 100, res = 300, width = 220, height = 300, units = "mm", pointsize = 11) # JPEG device

figure_spatial

dev.off()
    