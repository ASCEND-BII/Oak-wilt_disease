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

data <- fread("F:example_TSI.csv")
data$TSS_date <- as.IDate(data$TSS_date)
data$TSI_date <- as.IDate(data$TSI_date)
data$DOY <- yday(data$TSI_date)
data$r <- data$DOY/(365*2*pi)
data$pcx <- (data$TSI/10000)*cos(data$r)
data$pcy <- (data$TSI/10000)*sin(data$r)
data[TSI <= 0, pcx := 0]
data[TSI <= 0, pcy := 0]
plot(data$pcx, data$pcy)
plot(data$TSI_date, data$TSI)


ggplot(data) +
  geom_rect(aes(xmin= as.Date("2018-05-01"), xmax= as.Date("2018-10-18"),
                ymin=-Inf,ymax=Inf), alpha= 0.01, fill= "#1b9e77") +
  geom_rect(aes(xmin= as.Date("2019-05-01"), xmax= as.Date("2019-11-15"),
              ymin=-Inf,ymax=Inf), alpha= 0.01, fill= "#d95f02") +
  geom_rect(aes(xmin= as.Date("2020-05-05"), xmax= as.Date("2020-10-05"),
              ymin=-Inf,ymax=Inf), alpha= 0.01, fill= "#7570b3") +
  geom_hline(yintercept= 0, col = "grey", linetype = "dotted") +
  geom_point(aes(x = TSS_date, TSS/10000), shape = 21, size = 2, col = "white", fill = "black") +
  geom_line(aes(x = TSI_date, TSI/10000), col = "black", size = 0.5) +
  scale_x_date(date_breaks = "1 year", 
               date_labels = "%Y",
               expand = c(0,0),
               limits = as.Date(c('2018-01-01','2021-01-01'))) +
  th + ylab("CCI") + xlab("Year") 
  
#Polar metrics
polar <- data[, c("value", "r", "date")]
polar <- data[, c("TSI", "r", "TSI_date")]
polar <- na.exclude(polar)
polar[TSI <= 0, TSI := 0]
polar <- subset(polar, TSI_date >= as.IDate("2018-01-01"))
polar <- subset(polar, TSI_date <= as.IDate("2021-01-01"))
polar$Condition <- "a"
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

radial.plot(polar$value/10000, polar$r,
            clockwise = TRUE,
            start = s.pos,
            label.pos= lab.pos, 
            labels = rad.labs,
            radial.lim = c(0.0, 0.2, 0.4),
            radial.labels = c(0.0, 0.2, 0.4),
            rp.type = 's',
            point.symbols = 19, 
            point.col = polar$color,
            show.radial.grid= TRUE,
            main='CCI Polar Plot',
            grid.col='black',
            grid.unit= 'CCI')
