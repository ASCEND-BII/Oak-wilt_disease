library(data.table)
library(ggplot2)
library(ggpubr)
library(plotrix)

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

data <- fread("data/concept.csv")
data$TSS_date <- as.IDate(data$TSS_date)
data$TSI_date <- as.IDate(data$TSI_date)

plot(data$TSI_date, data$TSI)


ggplot(data) +
  geom_hline(yintercept= 0, col = "grey", linetype = "dotted") +
  geom_point(aes(x = TSS_date, TSS/10000), col = "grey") +
  geom_line(aes(x = TSI_date, TSI/10000), col = "black") +
  scale_x_date(date_breaks = "1 year", 
               date_labels = "%Y",
               expand = c(0,0),
               limits = as.Date(c('2018-01-01','2022-01-01'))) +
  th + ylab("CCI") + xlab("Year")






clrs <- colorRampPalette(c('blue3', 'gold'))(length(r)) # Color ramp for plot
# Polar plot of Willow Creek

polar <- data[, c("TSI", "r", "TSI_date")]
polar <- na.exclude(polar)
polar[TSI <= 0, TSI := 0]
polar <- subset(polar, TSI_date >= as.IDate("2018-01-01"))
polar <- subset(polar, TSI_date <= as.IDate("2022-01-01"))
polar$Condition <- NA
polar[year(TSI_date) > 2019, Condition := "Dead"]
polar[year(TSI_date) == 2019, Condition := "Wilted"]
polar[year(TSI_date) < 2019, Condition := "Healthy"]
polar$color <- "a"
polar[year(TSI_date) > 2019, color := "#7570b3"]
polar[year(TSI_date) == 2019, color := "#d95f02"]
polar[year(TSI_date) < 2019, color := "#1b9e77"]

lab.pos <- c(seq(from=0, to=2*pi-(2*pi/12), by=(2*pi)/12))[-4]
s.pos <- pi/2 # Radial position to start plotting from
rad.labs <- c(month.abb[seq(from=1, to=12)])[-4]

radial.plot(polar$TSI/10000, polar$r,
            clockwise = TRUE,
            start = s.pos,
            label.pos= lab.pos, 
            labels = rad.labs,
            radial.lim = c(0.0, 0.1, 0.2, 0.3),
            radial.labels = c(0.0, 0.1, 0.2, 0.3),
            rp.type = 's',
            point.symbols = 19, 
            point.col = polar$color,
            show.radial.grid= TRUE,
            main='CCI Polar Plot',
            grid.col='black',
            grid.unit= 'CCI')
