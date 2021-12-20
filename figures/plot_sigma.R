################################################################################
#' @title Plot of sigma of the Gaussian Process for training the algorithm
################################################################################

#-------------------------------------------------------------------------------
# Library
library(data.table)
library(ggplot2)
library(ggpubr)

#-------------------------------------------------------------------------------
# Arguments 
#' @param WV_sigma A data.table with the results of sigma search
#' @param NWV_sigma A data.table with the results of sigma search
#' @param NV_sigma A data.table with the results of sigma search

#-------------------------------------------------------------------------------
# Load
wilted_sigma <- fread("data/wilted_sigma.csv") 
healty_sigma <- fread("data/healty_sigma.csv") 
dead_sigma <- fread("data/dead_sigma.csv") 

#Add endmember and merge
wilted_sigma$endmember <- "Wilted"
healty_sigma$endmember <- "Healthy"
dead_sigma$endmember <- "Dead"

sigma <- rbind(healty_sigma, wilted_sigma, dead_sigma)

#Summary
sigma_summary <- sigma[, list(RMSE = mean(RMSE), RMSE_sd = sd(RMSE),
                              Rsquared = mean(Rsquared), Rsquared_sd = sd(Rsquared),
                              MAE = mean(RMSE), MAE_sd = sd(RMSE)),
                       by = c("endmember", "sigma")]

sigma_summary$endmember <- as.factor(sigma_summary$endmember)
sigma_summary$endmember <- ordered(sigma_summary$endmember, 
                                   levels = c("Healthy", "Wilted", "Dead"))

################################################################################
###Plot features----------------------------------------------------------------

th <- theme(
  panel.background = element_rect(fill = "transparent"),
  plot.background = element_rect(fill = "transparent", color = NA),
  panel.spacing = unit(0,"null"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(color = "transparent"))

###Plot-------------------------------------------------------------------------
min_sigma <- sigma_summary[, list(min = min(RMSE)), by = "endmember"]
healty <- sigma_summary[RMSE == min_sigma$min[1], sigma]
wilted <- sigma_summary[RMSE == min_sigma$min[2], sigma]
dead <- sigma_summary[RMSE == min_sigma$min[3], sigma]

#RMSE
sigma_RMSE <- ggplot(sigma_summary) +
  geom_ribbon(aes(x = sigma, ymin = RMSE-RMSE_sd, ymax = RMSE+RMSE_sd, fill = endmember), 
              alpha = 0.10) +
  geom_line(aes(x = sigma, y = RMSE, colour = endmember), 
            linetype = "solid") +
  scale_x_continuous(limits = c(0.0, 1.005), expand = c(0,0), n.breaks = 4) +
  scale_y_continuous(limits = c(0.2, 0.32), expand = c(0,0), n.breaks = 3) +
  scale_color_manual("Endmember", values = c("#1b9e77", "#e7298a", "#7570b3")) +
  scale_fill_manual("Endmember", values = c("#1b9e77", "#e7298a", "#7570b3")) +
  geom_vline(xintercept = healty, linetype= "dotted", colour = "#1b9e77") +
  geom_vline(xintercept = wilted, linetype= "dotted", colour = "#e7298a") +
  geom_vline(xintercept = dead, linetype= "dotted", colour = "#7570b3") +
  xlab("Sigma") + ylab("RMSE") +
  theme_bw() + th

#MAE
sigma_mae <- ggplot(sigma_summary) +
  geom_ribbon(aes(x = sigma, ymin = MAE-MAE_sd, ymax = MAE+MAE_sd, fill = endmember), 
              alpha = 0.10) +
  geom_line(aes(x = sigma, y = MAE, colour = endmember), 
            linetype = "solid") +
  scale_x_continuous(limits = c(0.0, 1.005), expand = c(0,0), n.breaks = 4) +
  scale_y_continuous(limits = c(0.2, 0.32), expand = c(0,0), n.breaks = 3) +
  scale_color_manual("Endmember", values = c("#1b9e77", "#e7298a", "#7570b3")) +
  scale_fill_manual("Endmember", values = c("#1b9e77", "#e7298a", "#7570b3")) +
  geom_vline(xintercept = healty, linetype= "dotted", colour = "#1b9e77") +
  geom_vline(xintercept = wilted, linetype= "dotted", colour = "#e7298a") +
  geom_vline(xintercept = dead, linetype= "dotted", colour = "#7570b3") +
  xlab("Sigma") + ylab("MAE") +
  theme_bw() +th

#R2
sigma_rsquared <- ggplot(sigma_summary) +
  geom_ribbon(aes(x = sigma, ymin = Rsquared-Rsquared_sd, ymax = Rsquared+Rsquared_sd, fill = endmember), 
              alpha = 0.10) +
  geom_line(aes(x = sigma, y = Rsquared, colour = endmember), 
            linetype = "solid") +
  scale_x_continuous(limits = c(0.0, 1.005), expand = c(0,0), n.breaks = 4) +
  scale_y_continuous(limits = c(0.39, 1), expand = c(0,0), n.breaks = 3) +
  scale_color_manual("Endmember", values = c("#1b9e77", "#e7298a", "#7570b3")) +
  scale_fill_manual("Endmember", values = c("#1b9e77", "#e7298a", "#7570b3")) +
  geom_vline(xintercept = healty, linetype= "dotted", colour = "#1b9e77") +
  geom_vline(xintercept = wilted, linetype= "dotted", colour = "#e7298a") +
  geom_vline(xintercept = dead, linetype= "dotted", colour = "#7570b3") +
  xlab("Sigma") + ylab(bquote("R"^2)) +
  theme_bw() + th 

###Arrange plot-----------------------------------------------------------------
yo <- ggarrange(sigma_RMSE,
                sigma_mae,
                sigma_rsquared,
                nrow = 3,
                ncol = 1,
                labels = c("a", "b", "c"), 
                font.label=list(color="black", size= 12, face = "plain"),
                common.legend = TRUE,
                widths = c(3), heights = c(3, 3, 3))

###Export-----------------------------------------------------------------------
jpeg("Figure_sigma.jpg", width = 12, height = 15, units = "cm", res = 600, pointsize = 7)

yo

dev.off()


