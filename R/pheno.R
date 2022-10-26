
data <- rast("F:/TRAINING/level3_ts/X0016_Y0024/2016-2022_001-365_HL_TSA_SEN2L_CCI_TSI.tif")
data <- subset(data, 24:46)
data <- as.data.table(as.data.frame(data, xy = TRUE, na.rm = TRUE))
dataaa <- data

val <- as.numeric(as.vector(dataaa[800, 3:25]))

data <- fread("F:/TRAINING/level3_ts-pixels/master_observations.csv")
data <- subset(data, ID == 100)
data <- subset(data, tile == "X0014_Y0024")
data <- subset(data, VI == "CCI")
data <- subset(data, date >= as.IDate("2016-09-01") &
                     date <= as.IDate("2022-02-01"))

plot(data$date, data$value)
data$doy <- yday(data$date)

r = (data$doy[8:30]/365.0)*(2.0*pi)

rad   = r
val   = data$value[8:30]
val   = val - min(val, na.rm = TRUE) + 1
val <- (val-min(val)) / (max(val)-min(val)) * 100

plot(data$doy[8:30], val)

year  = year(data$date[8:30])
doy   = data$doy[8:30]
ce    = year*365+doy 
pcx   = (val *cos(r))
pcy   = (val *sin(r))

plot(doy, val)
plot(pcx, pcy)

x <- mean(pcx, na.rm = TRUE)
y <- mean(pcy, na.rm = TRUE)

points(mean(pcx, na.rm = TRUE), mean(pcy, na.rm = TRUE), col = "red")

v <- sqrt(x^2 + y^2)
r <- atan2(y, x)
if(r > 0) {
  r = r
} else {
  r = r + 2*pi
}

if(r < pi) {
  angle = r + pi
} else {
  angle = r - pi
}

rad2d(r, dpc = 365)


cum <- cumsum(val)
cum <- (cum-min(cum)) / (max(cum)-min(cum)) * 100



plot(doy, cum)
abline(v = rad2d(r, dpc = 365))
abline(h = 85)
abline(h = 15)
plot(doy, val)
abline(v = rad2d(r, dpc = 365))
