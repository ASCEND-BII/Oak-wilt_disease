################################################################################
###### Hypothesis figure
################################################################################

library(ggplot2)
library(data.table)
library(lubridate)

data <- fread("data/hypothesis.csv")
data$date <- as.Date(data$date)
data$doy <- yday(data$date)

plot <- ggplot(data) +
  geom_smooth(aes(doy, healty, colour = condition), method = "lm", formula =  y ~ poly(x, 20), se = FALSE, alpha = 0.2) +
  geom_point(aes(doy, healty, fill = condition), colour = "white", shape = 21, size = 1.5) +
  xlab("Day of the Year") + ylab("Vegetation Index (e.g. NDVI)") +
  labs(title = "A possible scenario of trees with oak-wilt") +
  theme_bw(base_size = 10) +
  scale_x_continuous(limits = c(1, 366), expand=c(0,0), n.breaks = 7) +
  scale_y_continuous(limits = c(0, 1), expand=c(0,0), n.breaks = 5) +
  scale_fill_manual("Condition", values = c("#01665e", "#8c510a")) +
  scale_colour_manual("Condition", values = c("#35978f", "#bf812d"))

ggsave(plot = plot, 
       filename = "oak_hypothesis.jpeg", 
       width = 15,
       height = 8,
       units = "cm",
       device = "jpeg")  
