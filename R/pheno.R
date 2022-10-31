library(data.table)
library(terra)

data <- rast("/media/antonio/antonio_ssd/TRAINING/level3_ts/X0016_Y0024/2016-2022_001-365_HL_TSA_SEN2L_CCI_TSI.tif")
data <- as.data.table(as.data.frame(data, xy = TRUE, na.rm = FALSE))
ts <- data[, 26:139]
data <- data[, 1:2]
date <- colnames(ts)
date <- as.IDate(paste0(substr(date, 1, 4), "-", substr(date, 5, 6), "-", substr(date, 7, 8)))
doy <- yday(date)
year <- year(date)
doyce <- year(date)*365+doy

sample <- ts[1, ]
sample <- as.numeric(as.vector(sample))

n <- normalize(sample, min(sample), max(sample)) 
r <- doy2rad(doy) #r
vx <- pcx(n, r) #VX
vy <- pcy(n, r) #VY
xav <- xmean(vx) #vx
yav <- ymean(vy) #vy
rv_ang <- displacement(yav, xav) #rv_ang
rv_doy <- rad2doy(rv_ang) #av_doy
rv_mag <- distance(yav, xav) #rv_mag
ang_lest <- lest_activity_angle(rv_ang)
doy_lest <- rad2doy(ang_lest)
phenological <- season(doy, doy_lest)

a <- phenoloical_years(as.matrix(ts[1:10]), doy, 16)

phenological <- a[1,]

vi_test <- vi_year(sample, 5, phenological)
doy_test <- doy_year(doy, 5, phenological)
doy_shifted <- doy_corrected(doy_test, doy_lest, doy_delta = 16)
vi_min <- min(vi_test)
vi_max <- max(vi_test)
normaliz <- normalize(as.vector(vi_test), vi_min, vi_max)
peak <- peak_activity(as.vector(normaliz), as.vector(doy_shifted))
cum <- cumulative_observations(as.vector(normaliz))
cum_interpolated <- interpolation_cumulative(as.vector(cum), doy_shifted, 1:365)
vi_interpolated <- interpolation_vi(as.vector(vi_test), doy_shifted, 1:365)
doy_oi <- key_doys(as.vector(cum_interpolated), 15, 80, as.integer(peak[3]))
slopes <- get_slopes(as.vector(cum_interpolated), as.vector(vi_interpolated), as.vector(doy_oi))
values <- get_values(as.vector(doy_oi), as.vector(vi_test), as.vector(doy_shifted), 1:365)


plot(doy_test, vi_test)
abline(v = 165)

