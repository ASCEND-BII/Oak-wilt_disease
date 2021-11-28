NDVI


Normalized Difference Vegetation Index

base <- ts
ts


ts$NDVI <- (ts$NIR - ts$RED) / (ts$NIR + ts$RED)
ts$EVI <- 2.5 * ((ts$NIR - ts$RED) / (ts$NIR + 6 * ts$RED - 2.7 * ts$BLUE + 1))
ts$NBR <- (ts$NIR - ts$SWIR2) / (ts$NIR + ts$SWIR2)
ts$NDTI <- (ts$SWIR1 - ts$SWIR2) / (ts$SWIR1 + ts$SWIR2)
ts$ARVI <- (ts$NIR - (ts$RED - (ts$BLUE - ts$RED))) / (ts$NIR + (ts$RED - (ts$BLUE - ts$RED)))
ts$SAVI <- (ts$NIR - ts$RED) / (ts$NIR + ts$RED + 0.5) * (1 + 0.5)
ts$SARVI <- (ts$NIR - (ts$RED - (ts$BLUE - ts$RED))) / (ts$NIR + (ts$RED - (ts$BLUE - ts$RED)) + 0.5) * (1 + 0.5)
ts$TC_BRIGHT <- 0.2043*ts$BLUE + 0.4158*ts$GREEN + 0.5524*ts$RED + 0.5741*ts$NIR + 0.3124*ts$SWIR1 + 0.2303*ts$SWIR2
ts$TC_GREEN <- -0.1603*ts$BLUE - 0.2819*ts$GREEN - 0.4934*ts$RED + 0.7940*ts$NIR - 0.0002*ts$SWIR1 - 0.1446*ts$SWIR2
ts$TC_WET <- 0.0315*ts$BLUE + 0.2021*ts$GREEN + 0.3102*ts$RED + 0.1594*ts$NIR - 0.6806*ts$SWIR1 - 0.6109*ts$SWIR2
ts$TC_DI <- ts$TC_BRIGHT - (ts$TC_GREEN + ts$TC_WET)
ts$NDBI <- (ts$SWIR1 - ts$NIR) / (ts$SWIR1 + ts$NIR)
ts$NDWI <- (ts$GREEN - ts$NIR) / (ts$GREEN + ts$NIR)
ts$MNDWI <- (ts$GREEN - ts$SWIR1) / (ts$GREEN + ts$SWIR1)
ts$NDMI <- (ts$NIR - ts$SWIR1) / (ts$NIR + ts$SWIR1)
ts$NDSI <- (ts$GREEN - ts$SWIR1) / (ts$GREEN + ts$SWIR1)
ts$kNDVI <- (1 - exp( -(ts$NIR - ts$RED)^2 / (2 * (0.5 * (ts$NIR + ts$RED))^2) )) / (1 + exp( -(ts$NIR - ts$RED)^2 / (2 * (0.5 * (ts$NIR + ts$RED))^2) ))
ts$NDRE1 <- (ts$REDEDGE2 - ts$REDEDGE1) / (ts$REDEDGE2 + ts$REDEDGE1)
ts$NDRE2 <- (ts$REDEDGE3 - ts$REDEDGE1) / (ts$REDEDGE3 + ts$REDEDGE1)
ts$CIre <- (ts$REDEDGE3 / ts$REDEDGE1) - 1
ts$NDVIre1 <- (ts$BROADNIR - ts$REDEDGE1) / (ts$BROADNIR + ts$REDEDGE1)
ts$NDVIre2 <- (ts$BROADNIR - ts$REDEDGE2) / (ts$BROADNIR + ts$REDEDGE2)
ts$NDVIre3 <- (ts$BROADNIR - ts$REDEDGE3) / (ts$BROADNIR + ts$REDEDGE3)
ts$NDVIre1n <- (ts$NIR - ts$REDEDGE1) / (ts$NIR + ts$REDEDGE1)
ts$NDVIre2n <- (ts$NIR - ts$REDEDGE2) / (ts$NIR + ts$REDEDGE2)
ts$NDVIre3n <- (ts$NIR - ts$REDEDGE3) / (ts$NIR + ts$REDEDGE3)
ts$MSRre <- ((ts$BROADNIR / ts$REDEDGE1) - 1) / sqrt((ts$BROADNIR / ts$REDEDGE1) + 1)
ts$MSRren <- ((ts$NIR / ts$REDEDGE1) - 1) / sqrt((ts$NIR / ts$REDEDGE1) + 1)

fwrite(ts, "ts.csv")
