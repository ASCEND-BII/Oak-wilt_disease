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

summary_results <- summarySE(models_results, measurevar = "value", groupvars = c("Model", "Type", "variable"))



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

plot <- ggplot(data = models_results) + 
  geom_quasirandom(aes(x = value, y= Model, fill = Model, group = Type), shape = 21, linewidth=2, dodge.width = 0.75, color="grey", alpha = 0.5, show.legend = F) +
  geom_errorbar(data = summary_results, aes(xmin=value-sd, xmax=value+sd, y = Model, colour = Type), width=.0, position = position_dodge(0.75)) +
  geom_point(data = summary_results, aes(x = value, y = Model, colour = Type), fill = "white", shape = 21, size = 2, position = position_dodge(0.75)) +
  scale_shape_manual(
    name = "", labels = c("Training", "Testing"), values = rep(21, 8)) +
  scale_fill_viridis_d(option = "D", direction = 1) +
  scale_colour_manual(values = c("red", "black")) +
  theme_bw() + th + xlab("Value") +
  facet_wrap(~ variable, scales = "free_x")
  

#Export figure

jpeg("figures/model_comparisons.jpeg", quality = 100, res = 300, width = 210, height = 100, units = "mm", pointsize = 12) # JPEG device

plot

dev.off()
