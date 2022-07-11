################################################################################
#### Figure for Empirical Receiver Operating Characteristic Curve. 
################################################################################

#-------------------------------------------------------------------------------
# Libraries

library(ggplot2)
library(plotROC)

#-------------------------------------------------------------------------------
# Reading 

models <- readRDS("data/models/models.rds")

# Plot -------------------------------------------------------------------------

plot <- ggplot() + 
  geom_roc(data = models$wilted$pred, 
           aes(m=wilted, d=factor(obs, levels = c("wilted", "non_wilted"))),
           colour = "#7a0019") + 
  geom_roc(data = models$healthy$pred, 
           aes(m= non_healthy, d=factor(obs, levels = c("healthy", "non_healthy"))),
           colour = "#228b22") + 
  geom_roc(data = models$dead$pred, 
           aes(m= non_dead, d=factor(obs, levels = c("dead", "non_dead"))),
           colour = "#777677") + 
  coord_equal() +
  #style_roc() +
  geom_abline(intercept = 0, slope = 1, colour = "#d5d6d2", linetype = "dotted") +
  xlab("False positive fraction") + ylab("True positive fraction") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  labs(colour = "Condition")

#Export
tiff("figures/Figure_roc.tif", width = 12, height = 12, units = "cm", res = 600)

plot

dev.off()
