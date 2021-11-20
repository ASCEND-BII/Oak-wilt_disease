################################################################################
####### Time series analysis with gdalcubes
################################################################################

#Load libraries
library(gdalcubes)
library(sf)

remotes::install_github('r-spatial/sf', configure.args=("--with-gdal-config=/usr/bin/gdal-config --with-proj-lib=/usr/bin/proj --with-proj-include=/usr/include --with-proj-share=/usr/share/proj --with-proj-data=/usr/share/doc"))
