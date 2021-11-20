###############################################################################
######### Run force for Oak-wilt project
###############################################################################

###Validate Projection---------------------------------------------------------
USA_Contiguous_Lambert_Conformal_Conic

gdalsrsinfo -v PROJCS["USA_Contiguous_Lambert_Conformal_Conic",GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137,298.257222101]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Lambert_Conformal_Conic"],PARAMETER["False_Easting",0],PARAMETER["False_Northing",0],PARAMETER["Central_Meridian",-96],PARAMETER["Standard_Parallel_1",33],PARAMETER["Standard_Parallel_2",45],PARAMETER["Latitude_Of_Origin",39],UNIT["Meter",1]]

###Landsat-8 Archive processing---------------------------------------------------

#Running force on Landsat-8 archive
force-level2 /media/antonio/Work/Oak-Sentinel/param/L2_oak-wilt_landsat.prm

#Report of Landsat-8
force-level2-report /media/antonio/Work/Oak-Sentinel/log_landsat

#Time series Landsat-8 NIR for coregistration
force-higher-level /media/antonio/Work/Oak-Sentinel/param/L3_landsat-tsa.prm

#Create mosaic
force-mosaic /media/antonio/Work/Oak-Sentinel/level3

###Sentinel-2 Archive processing---------------------------------------------------------------
#Build for archive
force-level2 /media/antonio/Work/Oak-Sentinel/param/L2_oak-wilt_sen2.prm

#Report of Sentinel-2
force-level2-report /media/antonio/Work/Oak-Sentinel/log_sen2

#Time series Sentinel-2 for time series visualization
force-higher-level /media/antonio/Work/Oak-Sentinel/param/L3_sentinel-tsa.prm

###Create a mask-------------------------------------------------------------------------------
force-cube /media/antonio/Work/Oak-Sentinel/misc/mask/aos.gpkg /media/antonio/Work/Oak-Sentinel/misc/mask rasterize 10
force-mosaic /media/antonio/Work/Oak-Sentinel/misc/mask

###Evaluate log report files--------------------------------------------------
###Create a level2 report of log files
force-level2-report /media/antonio/Work/Oak-Sentinel/log

###Create grid of datacube
force-tabulate-grid -b 42.491920000,49.38435799999,-97.2290390000,-86.7816795053 -f shp /media/antonio/Work/Oak-Sentinel/level2

###Create mask

###

sudo docker docker run -v /media/antonio/Work/Oak-Sentinel:/opt davidfrantz/force:dev force-higher-level /opt/param/L3_sentinel-tsa_docker.prm













X0037-Y0046
QUEUED
