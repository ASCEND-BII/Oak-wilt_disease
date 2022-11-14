################################################################################
##### 01 - Conceptual figure of phenology
################################################################################

#' @description A script for ploting the conceptual figure of phenology in the 
#' context of oak wilt.

#-------------------------------------------------------------------------------
#' Library

library(data.table)
library(ggplot2)
library(ggpubr)
library(plotrix)
library(PolarMetrics)

#-------------------------------------------------------------------------------
#' Plot features
pa <- c("#1b9e77", "#d95f02", "#7570b3")
tamano <- 14
tamano2 <- 12
th <- theme_bw(base_size = tamano) + theme(plot.background = element_blank(),
                                           panel.grid.major = element_blank(),
                                           panel.grid.minor = element_blank(),
                                           plot.margin = margin(6, 12, 0, 0, "pt"),
                                           axis.text.x = element_text(color = "black", size = tamano2),
                                           axis.text.y = element_text(color = "black", size = tamano2),
                                           strip.text.x = element_text(size = tamano, color = "black"),
                                           strip.text.y = element_text(size = tamano, color = "black"),
                                           strip.background = element_rect(color= "black", fill="grey90", linetype="solid"))

#-------------------------------------------------------------------------------
#' Plot time series

data <- fread("data/concept.csv")
data$TSS_date <- as.IDate(data$TSS_date)
data$TSI_date <- as.IDate(data$TSI_date)

data_TSS <- data[, 1:2]
data_TSS <- na.exclude(data_TSS)
data_TSI <- data[, 3:4]
data_TSI <- na.exclude(data_TSI)

jpeg("02-ts.jpeg", quality = 100, res = 300, width = 210, height = 80, units = "mm", pointsize = 12) # JPEG device

ggplot() +
      geom_rect(aes(xmin = as.IDate("2018-05-01"), xmax = as.IDate("2018-10-17"), ymin = -Inf, ymax = Inf), fill = pa[1], colour = NA, alpha = 0.2) +
      geom_rect(aes(xmin = as.IDate("2019-05-02"), xmax = as.IDate("2019-10-20"), ymin = -Inf, ymax = Inf), fill = pa[2], colour = NA, alpha = 0.2) +
      geom_rect(aes(xmin = as.IDate("2020-05-04"), xmax = as.IDate("2020-10-05"), ymin = -Inf, ymax = Inf), fill = pa[3], colour = NA, alpha = 0.2) +
      geom_hline(yintercept= 0, col = "grey", linetype = "dotted") +
      geom_point(data = data_TSS, aes(x = TSS_date, TSS/10000), col = "white", fill = "grey", shape = 21, size = 2) +
      geom_line(data = data_TSI, aes(x = TSI_date, TSI/10000), col = "black", size = 0.4) +
      scale_x_date(date_breaks = "1 year", 
               date_labels = "%Y",
               expand = c(0,0),
               limits = as.Date(c('2018-01-01','2021-01-01'))) +
      scale_y_continuous(expand = c(0,0),
                         limits = c(-0.25, 0.5)) +
      ylab("CCI") + xlab("Year") +
      annotate("text", label = "Healthy", x = as.IDate("2018-07-30"), y = -0.17, size = 5, colour = pa[1]) +
      annotate("text", label = "Diseased", x = as.IDate("2019-07-30"), y = -0.17, size = 5, colour = pa[2]) +
      annotate("text", label = "Dead", x = as.IDate("2020-07-25"), y = -0.17, size = 5, colour = pa[3]) +
      th

dev.off()

#-------------------------------------------------------------------------------
#' Radial plot

polar <- subset(data_TSI, TSI_date >= as.IDate("2018-01-01"))
polar <- subset(polar, TSI_date <= as.IDate("2021-01-01"))
polar[TSI <= 0, TSI := 0]
polar$Condition <- "a"
polar[year(TSI_date) > 2019, Condition := "Dead"]
polar[year(TSI_date) == 2019, Condition := "Diseased"]
polar[year(TSI_date) < 2019, Condition := "Healthy"]
polar$color <- "a"
polar[year(TSI_date) > 2019, color := pa[3]]
polar[year(TSI_date) == 2019, color := pa[2]]
polar[year(TSI_date) < 2019, color := pa[1]]

#For polar plot
dpy <- 365                 # Days/year
c <- 21                    # Num. of years/cycles
t <- as.vector(polar$TSI_date)  # Days since January 1, 2000
r <- t2rad(t, dpc=dpy)     # Transform days of year to radians
v <- as.vector(polar$TSI/10000)   # MODIS NDVI for Willow Creek flux tower, WI
v[v <= 0] <- 0
vx <- mean(vec.x(r,v), na.rm=TRUE) # Avg horizontal vector
vy <- mean(vec.y(r,v), na.rm=TRUE) # Avg vertical vector
rv <- vec_mag(vx,vy)       # Magnitude (length) of average vector
rv_ang <- vec_ang(vx,vy)   # Angle of the avg vector (phenological median)
avec_ang <- avec_ang(rv_ang)   # Vert opposite of med (avg NDVI min/pheno yr start)
cxs <- 1                   # Text scaling in plots

lab.pos <- c(seq(from=0, to=2*pi-(2*pi/12), by=(2*pi)/12))[-4]
s.pos <- pi/2 # Radial position to start plotting from
rad.labs <- c(month.abb[seq(from=1, to=12)])[-4]

jpeg("02-radial.jpeg", quality = 100, res = 300, width = 105, height = 80, units = "mm", pointsize = 12) # JPEG device

radial.plot(v,
            r,
            clockwise = TRUE,
            start = s.pos,
            label.pos= lab.pos, 
            labels = rad.labs,
            radial.lim = c(0.0, 0.2, 0.4),
            radial.labels = c("", 0.2, 0.4),
            rp.type = 's',
            point.symbols = 19, 
            point.col = polar$color,
            show.radial.grid= TRUE,
            grid.col='black',
            grid.unit= 'CCI')
#radial.plot(c(0,0.4),c(0,rv_ang),
#            clockwise=TRUE,start=s.pos,rp.type='r',
#            lwd=2, lty = 1, line.col='gray45',add=TRUE) # rv_ang, Angle of avg vec
radial.plot(c(0,0.4),c(0, avec_ang),
            clockwise=TRUE,start=s.pos,rp.type='r',
            lwd=1, lty = 2, line.col='gray45',add=TRUE) # rv_ang, Angle of avg vec
radial.plot(c(0,0.4),c(0,avec_ang),
            clockwise=TRUE,start=s.pos,rp.type='s',
            point.symbols='*',cex=cxs*2,
            add=TRUE) # avec_ang, opposite angle of rv_ang
radial.plot(c(0,rv),c(rv_ang,rv_ang),
            clockwise=TRUE,start=s.pos,rp.type='r',
            lwd=3,line.col='red',add=TRUE)		# rv, Magnitude of avg vec
radial.plot(c(0,rv),c(rv_ang,rv_ang),
            clockwise=TRUE,start=s.pos,rp.type='s',
            point.symbols='*',cex=cxs*2,
            point.col='red',add=TRUE)   # rv, Magnitude of avg vec
text(0.15, -0.05,'Average vector',col='red',cex= 0.5) # Add text label
#text(-0.06, -0.32, 'Mid season', col='gray45', cex= 0.5) # Add text label
text(0.10, 0.32, 'Offset', col='gray45', cex= 0.5) # Add text label

dev.off()

#-------------------------------------------------------------------------------
#' Accumulative plot

data_cum <- data[, 9:12]
data_cum <- na.exclude(data_cum)
data_cum[Healthy < 0, Healthy := 0]
data_cum[Diseased < 0, Diseased := 0]
data_cum[Dead < 0, Dead := 0]

#Cumsum
data_cum$Healthy <- cumsum(data_cum$Healthy)/max(cumsum(data_cum$Healthy))
data_cum$Diseased <- cumsum(data_cum$Diseased)/max(cumsum(data_cum$Diseased))
data_cum$Dead <- cumsum(data_cum$Dead)/max(cumsum(data_cum$Dead))

jpeg("02-cumsum.jpeg", quality = 100, res = 300, width = 105, height = 80, units = "mm", pointsize = 12) # JPEG device

ggplot(data_cum) +
  geom_point(aes(x = DOY, y = Healthy), col = "white", fill = pa[1], shape = 21, size = 2) +
  geom_point(aes(x = DOY, y = Diseased), col = "white", fill = pa[2], shape = 21, size = 2) +
  geom_point(aes(x = DOY, y = Dead), col = "white", fill = pa[3], shape = 21, size = 2) +
  geom_line(aes(x = DOY, y = Healthy), col = pa[1], size = 0.4) +
  geom_line(aes(x = DOY, y = Diseased), col = pa[2], size = 0.4) +
  geom_line(aes(x = DOY, y = Dead), col = pa[3], size = 0.4) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(-0.01, 1.01)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 365), breaks = c(1, 100, 200, 300, 365)) +
  th + ylab("Cumulative CCI") + xlab("Day of the Phenological Year") +
  geom_hline(yintercept = 0.15, linetype= "dashed", color = "grey") +
  geom_hline(yintercept = 0.85, linetype= "dashed", color = "grey") +
  annotate("segment", x = 40, xend = 40, y = 0.15, yend = 0.85,
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm")), color = "grey20") +
  annotate("text", label = "Green Season", x = 30, y = 0.50, size = 3, color = "grey20", angle='90')
  
dev.off()
