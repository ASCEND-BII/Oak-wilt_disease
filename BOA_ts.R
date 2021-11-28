################################################################################
####### BOA time series extraction and plot
################################################################################

#Library
library(data.table)
library(raster)
library(ggplot2)
library(RStoolbox)
library(scales)
library(ggpubr)

#Read points
points <- fread("/media/antonio/Work/Oak-Sentinel/misc/oak_trees/test.csv")

#Path of BOA
path <- "/media/antonio/Work/Oak-Sentinel/level2/X0031_Y0047"
path_elements <- length(strsplit(path, split = "/")[[1]])+1

#Find BOA products
BOA <- list.files(path = path, 
                    pattern = "_BOA.tif", 
                    all.files = TRUE,
                    full.names = TRUE, 
                    recursive = TRUE,
                    include.dirs = TRUE)

BOA2 <- list.files(path = "/media/antonio/Work/Oak-Sentinel/level2/X0031_Y0048", 
                  pattern = "_BOA.tif", 
                  all.files = TRUE,
                  full.names = TRUE, 
                  recursive = TRUE,
                  include.dirs = TRUE)

BOA <- c(BOA, BOA2)

###Create frame for extraction of information
frame <- data.table(files = BOA)

#Date
frame[ , date := as.Date(paste0(substr(strsplit(files, split = "/")[[1]][path_elements], 1, 4), "-",
                                substr(strsplit(files, split = "/")[[1]][path_elements], 5, 6), "-",
                                substr(strsplit(files, split = "/")[[1]][path_elements], 7, 8))),
       by = seq_len(nrow(frame))]
frame <- frame[order(date)]

#Sensor
frame[ , sensor := substr(strsplit(files, split = "/")[[1]][path_elements], 17, 21),
       by = seq_len(nrow(frame))]

#Subset for Sentinel-2
frame <- subset(frame, sensor != "LND08")

###Read and plot function-------------------------------------------------------

#Color gradient function
col <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", 
         "#f5f5f5", "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30")

col2 <- c("#762a83", "#9970ab",
  "#c2a5cf", "#e7d4e8",
  "#d9f0d3", "#a6dba0",
  "#5aae61", "#1b7837")

#Function video
BOA_ts <- function(frame, out_path = "data/temp", points = points) {
  
  xy <- points[,c(1,2)]
  
  spdf <- SpatialPointsDataFrame(coords = xy, data = points,
                                 proj4string = CRS("+proj=lcc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
  
  #TS to storage the values
  ts <- data.table()
  
  date_unique <- unique(frame$date)
  
  pb <- txtProgressBar(min = 0, max = length(date_unique), style = 3)
  
  for(i in 1:length(date_unique)) {
    
    sub <- subset(frame, date == date_unique[i])
    
    setTxtProgressBar(pb, i)
    
    #Read and create NDVI
    if(nrow(sub) == 1 ) {
      
      scene <- raster::stack(sub$files[1])
      
    } else {
      
      scene1 <- raster::stack(sub$files[1])
      names(scene1)
      scene2 <- raster::stack(sub$files[2])
      scene <- mosaic(scene1, scene2, fun = min)
      names(scene) <- names(scene1)
    }
    
    NDVI <- (scene$NIR - scene$RED) / (scene$NIR + scene$RED)
    NDRE2 <- (scene$REDEDGE3 - scene$REDEDGE1) / (scene$REDEDGE3 + scene$REDEDGE1)
    
    #Extract values
    values <- extract(scene, spdf)
    values <- data.table(date = date_unique[i],
                         Class = points$Status,
                         values)
    
    #Storage values
    ts <- rbind(ts, values)
    
    values$NDVI <- (values$NIR - values$RED) / (values$NIR + values$RED)
    values$NDRE <- (values$REDEDGE3 - values$REDEDGE1) / (values$REDEDGE3 + values$REDEDGE1)
    
    values <- na.exclude(values)
    
    #TS plot
    RGB <-  ggRGB(scene, r = 3, g = 2, b = 1, scale = 10000,
                  stretch = "lin", ext = NULL, limits = NULL,
                  clipValues = "limits", quantiles = c(0.02, 0.98), ggObj = TRUE,
                  ggLayer = FALSE, alpha = 1, coord_equal = TRUE,
                  geom_raster = TRUE, nullValue = 0) +
      scale_x_continuous(limits = c(189500, 192000), expand=c(0,0), n.breaks = 4) +
      scale_y_continuous(limits = c(722500, 724000), expand=c(0,0), n.breaks = 4) +
      xlab("X (m)") + ylab("Y (m)") +
      labs(subtitle = paste0(date_unique[i], "   ", frame$sensor[i])) +
      geom_point(data = points, aes(x= x, y= y, shape = Status, colour = Status), size = 2) +
      scale_shape_manual("Class", values=c(21, 23, 24, 25, 22), breaks = c('Healthy','Wilted','Evergreen', 'Ground', "Water")) +
      scale_colour_manual("Class", values=c('#228B22', "red", "orange", "black", "blue"), breaks = c('Healthy','Wilted','Evergreen', 'Ground', "Water"))
    
    plot_kNDVI <- ggR(NDVI, geom_raster = TRUE) +
      scale_x_continuous(limits = c(189500, 192000), expand=c(0,0), n.breaks = 4) +
      scale_y_continuous(limits = c(722500, 724000), expand=c(0,0), n.breaks = 4) +
      scale_fill_gradientn("NDVI", colours= col, limits = c(-1, 1), n.breaks = 4, oob = scales::squish) +
      xlab("X (m)") + ylab("Y (m)") +
      labs(subtitle = "NDVI") +
      geom_point(data = points, aes(x= x, y= y, shape = Status, colour = Status), size = 2) +
      scale_shape_manual("Class", values=c(21, 23, 24, 25, 22), breaks = c('Healthy','Wilted','Evergreen', 'Ground', "Water")) +
      scale_colour_manual("Class", values=c('#228B22', "red", "orange", "black", "blue"), breaks = c('Healthy','Wilted','Evergreen', 'Ground', "Water"))
    
    plot_NDMI <- ggR(NDRE2, geom_raster = TRUE) +
      scale_x_continuous(limits = c(189500, 192000), expand=c(0,0), n.breaks = 4) +
      scale_y_continuous(limits = c(722500, 724000), expand=c(0,0), n.breaks = 4) +
      scale_fill_gradientn("NDRE", colours= col, limits = c(-1, 1), n.breaks = 4, oob = scales::squish) +
      labs(subtitle = "NDRE") +
      xlab("X (m)") + ylab("Y (m)") +
      geom_point(data = points, aes(x= x, y= y, shape = Status, colour = Status), size = 2) +
      scale_shape_manual("Class", values=c(21, 23, 24, 25, 22), breaks = c('Healthy','Wilted','Evergreen', 'Ground', "Water")) +
      scale_colour_manual("Class", values=c('#228B22', "red", "orange", "black", "blue"), breaks = c('Healthy','Wilted','Evergreen', 'Ground', "Water"))
    
    sub_values <- values[Class == "Healthy" | Class == "Wilted"]
    
    plot_points <- ggplot() + 
      geom_point(data = values, aes(x= NDVI, y= NDRE, shape = Class, colour = Class), size = 2) +
      labs(subtitle = paste0(date_unique[i], "   ", frame$sensor[i])) +
      scale_shape_manual("Class", values=c(21, 23, 24, 25, 22), breaks = c('Healthy','Wilted','Evergreen', 'Ground', "Water")) +
      scale_colour_manual("Class", values=c('#228B22', "red", "orange", "black", "blue"), breaks = c('Healthy','Wilted','Evergreen', 'Ground', "Water")) +
      scale_x_continuous(limits = c(-1, 1), expand=c(0,0), n.breaks = 4) +
      scale_y_continuous(limits = c(-1, 1), expand=c(0,0), n.breaks = 4) +
      stat_ellipse(data = sub_values, aes(x= NDVI, y= NDRE, colour = Class), type = "t", alpha = 0.5) +
      theme_bw()
    
    yo <- ggarrange(RGB,
                    plot_points,
                    plot_kNDVI,
                    plot_NDMI,
                    nrow = 2,
                    ncol = 2,
                    labels = c("a", "b", "c", "d"), 
                    common.legend = TRUE,
                    widths = c(3, 3), heights = c(3, 3)) 
    
    ggsave(plot = yo, 
           filename = paste0(out_path, "/", date_unique[i], ".jpeg"), 
           width = 10,
           height = 7,
           device = "jpeg",
           bg = "white")
    
  }
  ts
}

ts <- BOA_ts(frame, out_path = "data/temp", points = points)
fwrite(ts, "ts.csv")

#END

