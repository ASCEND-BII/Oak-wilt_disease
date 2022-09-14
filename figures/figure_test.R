#VSS
library(ggpubr)


data <- data[year == "2019"]

data <- data[condition != "wilted"]

col <- colnames(data)
col <- col[11:48]
n_col <- length(col)

for(i in 1:n_col) {
  
  select <- c("condition", "VI", col[i])
  
  frame <- data[, ..select]
  colnames(frame) <- c("condition", "VI", "value")
  
  CCI <- ggbetweenstats(
    data = frame[VI == "CCI"],
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
    title = "CCI"
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
  
  plot <- ggarrange(CCI, CRE, NDM, KNV,
                    ncol = 2, nrow = 2)
  
  name <- paste0("figures/", col[i], ".jpeg")
  
  ggsave(
    filename = name,
    plot = plot,
    width = 25,
    height = 25,
    units = "cm",
    dpi = 300,
    bg = "white")
  
}

