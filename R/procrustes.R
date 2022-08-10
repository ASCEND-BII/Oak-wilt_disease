library(shapes)

#Procrustes

frame <- subset(data, year(as.Date(date)) == 2019)
frame <- frame[, c(1, 3, 8, 9, 11)]
frame <- subset(frame, month(as.Date(date)) > 2)
frame <- subset(frame, month(as.Date(date)) < 10)

frame$CCI <- tanh((frame$CCI/10000)^2)
frame$CRE <- frame$CRE/1000
frame$NDW <- tanh((frame$NDW/10000)^2)

a <- frame[ID == 1]
plot3d(a[, 3:5])

#k x m x n real array, (or k x n complex matrix for m=2 is OK), where k is the number of points, m is the number of dimensions, and n is the sample size. 

to_transform <- array(NA, dim=c(length(unique(frame$date)), 3, length(unique(frame$ID))))

IDs <- unique(frame$ID)

for(n in 1:length(IDs)) {
  
  sub_frame <- subset(frame, ID == IDs[n])
  
  to_transform[, 1, n] <- sub_frame$CCI
  to_transform[, 2, n] <- sub_frame$CRE
  to_transform[, 3, n] <- sub_frame$NDW
  
} 

GPT <- procGPA(to_transform, 
               scale = FALSE, 
               reflect = FALSE, 
               eigen2d = FALSE, 
               tol1 = 1e-05, 
               tol2 = 1e-05, 
               tangentcoords = "residual", 
               proc.output = TRUE, 
               distances = TRUE, 
               pcaoutput = TRUE, 
               alpha=0, 
               affine=FALSE)

shapepca(GPT, type="r", mag=3)
