library(shapes)

#-------------------------------------------------------------------------------
# Root path

path <- "/media/antonio/antonio_ssd/FORCE"

#-------------------------------------------------------------------------------
# Reading and cleaning

#Reading -------------
X0014_Y0024 <- fread(paste0(path, "/level3_shifted/X0014_0024_VI_clean.txt"))
X0015_Y0024 <- fread(paste0(path, "/level3_shifted/X0015_0024_VI_clean.txt"))

#Add tiles
X0014_Y0024$tile <- "X0014_Y0024"
X0015_Y0024$tile <- "X0015_Y0024"
#X0016_Y0024$tile <- "X0016_Y0024"

#rbind tiles
data <- rbind(X0014_Y0024, X0015_Y0024)
#Procrustes

frame <- subset(data, year(as.Date(date)) == 2019)
info <- frame[, c(1, 2, 12)]
frame <- frame[, c(12, 1, 2, 3, 8, 9, 11)]
frame <- subset(frame, month(as.Date(date)) > 2)
frame <- subset(frame, month(as.Date(date)) < 10)

frame$CCI <- tanh((frame$CCI/10000)^2)
frame$CRE <- frame$CRE/1000
frame$NDW <- tanh((frame$NDW/10000)^2)

a <- frame[ID == 1]
plot3d(a[, 5:6])

#Unique convination
unique_IDs <- frame[, .N, by= c("tile", "ID")]
unique_IDs <- unique_IDs[N == 13]

#k x m x n real array, (or k x n complex matrix for m=2 is OK), where k is the number of points, m is the number of dimensions, and n is the sample size. 

to_transform <- array(NA, dim=c(3, length(unique(frame$date)), nrow(unique_IDs)))

for(n in 1:nrow(unique_IDs)) {
  
  sub_frame <- subset(frame, tile == unique_IDs$tile[n] & ID == unique_IDs$ID[n])
  
  to_transform[1, , n] <- sub_frame$CCI
  to_transform[2, , n] <- sub_frame$CRE
  to_transform[3, , n] <- sub_frame$NDW
  
} 

GPT <- procGPA(to_transform, 
               scale = TRUE, 
               reflect = FALSE, 
               eigen2d = FALSE, 
               tol1 = 1e-05, 
               tol2 = 1e-05, 
               tangentcoords = "residual", 
               proc.output = TRUE, 
               distances = TRUE, 
               pcaoutput = TRUE, 
               alpha= 0, 
               affine=FALSE)

shapepca(GPT, type="r", mag=3)
