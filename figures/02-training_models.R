################################################################################
##### 02 - Plot comparing training models (Fig. 4)
################################################################################

#' @description A script for ploting the comparison of training models.

#-------------------------------------------------------------------------------
# Libraries

library(data.table)
library(ggplot2)
library(ggbeeswarm)
library(Rmisc)

#-------------------------------------------------------------------------------
# Read values of evaluation

models_results <- fread("data/models_selection.csv")
models_results <- models_results[, c(1, 4, 6, 7)]
models_results <- melt(models_results, id.vars = c("Model", "Type"))

#Summary for ordination
mean_accuracy <- models_results[variable == "Accuracy", mean(value), by = "Model"]
mean_accuracy <- mean_accuracy[order(V1)]
levels <- mean_accuracy$Model

models_results$Model <- as.factor(models_results$Model)
models_results$Model <- factor(models_results$Model, 
                               levels = levels)

models_results$Type <- as.factor(models_results$Type)
models_results$Type <- factor(models_results$Type, 
                              levels = c("Testing", "Training"))

summary_results <- summarySE(models_results, measurevar = "value", groupvars = c("Model", "Type", "variable"))
summary_results$Type <- as.factor(summary_results$Type)
summary_results$Type <- factor(summary_results$Type, 
                              levels = c("Testing", "Training"))

summary_overall <- summarySE(models_results, measurevar = "value", groupvars = c("Type", "variable"))

#Layout properties -------------------------------------------------------------
pa <- c("#1b9e77", "#d95f02", "#7570b3")
tamano <- 12
tamano2 <- 11

th <- theme_bw(base_size = tamano) + theme(plot.background = element_blank(),
                                           panel.grid.major = element_blank(),
                                           panel.grid.minor = element_blank(),
                                           plot.margin = margin(4, 12, 0, 4, "pt"),
                                           axis.text.x = element_text(color = "black", size = tamano2),
                                           axis.text.y = element_text(color = "black", size = tamano2),
                                           strip.text.x = element_text(size = tamano, color = "black"),
                                           strip.text.y = element_text(size = tamano, color = "black"),
                                           strip.background = element_rect(color= "black", fill="grey90", linetype="solid"))

#-------------------------------------------------------------------------------
# Ploting comparison 

plot <- ggplot() + 
  geom_vline(data = summary_overall, 
             aes(xintercept = value, 
                 colour = Type),
             linetype = "dotted",
             show.legend = F) + 
  geom_quasirandom(data = models_results,
                   aes(x = value, 
                       y= Model, 
                       fill = Model, 
                       group = Type,
                       shape = Type), 
                   linewidth=2, 
                   dodge.width = 0.75, 
                   color="grey", 
                   alpha = 0.5, 
                   show.legend = F,
                   size = 1) +
  geom_errorbar(data = summary_results, 
                aes(xmin=value-sd, 
                    xmax=value+sd, 
                    y = Model, 
                    colour = Type), 
                width= .0, 
                position = position_dodge(0.75),
                show.legend = F) +
  geom_point(data = summary_results, 
             aes(x = value, 
                 y = Model, 
                 colour = Type,
                 shape = Type), 
             fill = "white", 
             size = 1.5, 
             alpha = 0.85,
             position = position_dodge(0.75)) +
  scale_x_continuous(expand = c(0.00, 0.0128), guide = guide_axis(check.overlap = TRUE)) +
  scale_shape_manual(name = "", values = c(24, 21)) +
  scale_fill_viridis_d(option = "D", direction = 1) +
  scale_colour_manual(values = c("red", "black")) +
  theme_bw() + th + xlab("Value") +
  facet_wrap(~ variable, scales = "free_x") +
  theme(legend.position="top")
  

#Export figure

jpeg("figures/model_comparisons.jpeg", quality = 100, res = 300, width = 190, height = 100, units = "mm", pointsize = 12) # JPEG device

plot

dev.off()
