#VSS
library(Rmisc)
library(ggplot2)
library(data.table)
library(ggpubr)
library(PupillometryR)


data <- fread(paste0(path, "/master_training.csv"))

data$Condition <- as.factor(data$Condition)
data$Condition <- factor(data$Condition, levels = c("Healthy", "Wilted", "Dead"))

data$year <- as.factor(data$year)
data$year <- factor(data$year, levels = c("2018", "2019", "2021"))

#Layout properties -------------------------------------------------------------
#pa <- c("#1b9e77", "#d95f02", "#7570b3")
pa <- c("#481568FF", "#1F968BFF", "#B8DE29FF")
tamano <- 14
tamano2 <- 12

th <- theme_bw(base_size = tamano) + theme(plot.background = element_blank(),
                                           panel.grid.major = element_blank(),
                                           panel.grid.minor = element_blank(),
                                           plot.margin = margin(4, 6, 0, 0, "pt"),
                                           axis.text.x = element_text(color = "black", size = tamano2),
                                           axis.text.y = element_text(color = "black", size = tamano2),
                                           strip.text.x = element_text(size = tamano, color = "black"),
                                           strip.text.y = element_text(size = tamano, color = "black"),
                                           strip.background = element_rect(color= "black", fill="grey90", linetype="solid"))

#Plot --------------------------------------------------------------------------

col <- colnames(data)
col <- col[9:length(col)]
n_col <- length(col)

for(i in 1:n_col) {
  
  select <- c("Condition", "year", col[i])
  val_name <- as.character(col[i])
  
  #Subset
  frame <- data[, ..select]
  colnames(frame) <- c("condition", "year", "value")
  frame <- frame[is.infinite(value) != TRUE]
  
  #Summary
  summary_gruops <- summarySE(frame, measurevar = "value",
                              groupvars=c("condition", "year"))
  
  summary_condition <- summarySE(frame, measurevar = "value",
                                 groupvars=c("condition"))
  
  summary_condition$lower_ci <- summary_condition$value - summary_condition$ci
  summary_condition$upper_ci <- summary_condition$value + summary_condition$ci
  summary_condition[, 2:8] <- round(summary_condition[, 2:8], 4)
  
  A <- ggplot() +
    #geom_hline(yintercept = 0, linetype= "longdash", colour = "grey75") +
    # geom_errorbar(data= frame, aes(x = year, ymax= value, ymin= value), colour = "grey20", linetype = 2)  
    geom_flat_violin(data = frame, aes(x = condition, y = value, fill = year), position = position_nudge(x = .08, y = 0), adjust = 1.5, trim = FALSE, alpha = .35, colour = "white") +
    geom_point(data = frame, aes(x = as.numeric(condition)-.15, y = value, colour = year, shape = condition), position = position_jitter(width = .05), size = .25, shape = 20) +
    geom_boxplot(data = frame, aes(x = condition, y = value, fill = year), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
    geom_line(data = summary_gruops, aes(x = as.numeric(condition)+.1, y = value, group = year, colour = year), linetype = 3) +
    geom_point(data = summary_gruops, aes(x = as.numeric(condition)+.1, y = value, group = year, colour = year), shape = 18) +
    scale_colour_manual(values = pa) +
    scale_fill_manual(values = pa) +
    th + xlab("Condition") + ylab(val_name) + coord_cartesian(ylim = c(0, 0.70), xlim = c(0.8, 3.5), expand = FALSE)
  
  
   
  
  name <- paste0("/media/antonio/antonio_ssd/TRAINING/figures/", col[i], ".jpeg")
  
  ggsave(
    filename = name,
    plot = A,
    width = 25,
    height = 25,
    units = "cm",
    dpi = 300,
    bg = "white")
  
}

KNV <- ggbetweenstats(
  data = frame,
  x = condition,
  y = value,
  type = "parametric", # ANOVA or Kruskal-Wallis
  var.equal = TRUE, # ANOVA or Welch ANOVA
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE,
  xlab = "Condition",
  ylab = col[i],
  title = "KNV"
)

CRE <- ggbetweenstats(
  data = frame[VI == "CRE"],
  x = condition,
  y = value,
  type = "parametric", # ANOVA or Kruskal-Wallis
  var.equal = TRUE, # ANOVA or Welch ANOVA
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE,
  xlab = "Condition",
  ylab = col[i],
  title = "CRE"
)

NDM <- ggbetweenstats(
  data = frame[VI == "NDM"],
  x = condition,
  y = value,
  type = "parametric", # ANOVA or Kruskal-Wallis
  var.equal = TRUE, # ANOVA or Welch ANOVA
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE,
  xlab = "Condition",
  ylab = col[i],
  title = "NDM"
)

KNV <- ggbetweenstats(
  data = frame[VI == "KNV"],
  x = condition,
  y = value,
  type = "parametric", # ANOVA or Kruskal-Wallis
  var.equal = TRUE, # ANOVA or Welch ANOVA
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE,
  xlab = "Condition",
  ylab = col[i],
  title = "KNV")

