## Data processing using FORCE

----

The imagery used to detect oak-wilt were processed thought 
[FORCE](https://force-eo.readthedocs.io/en/latest/) (version 3.7.4). 

The data processing was conducted using a Ubuntu 20.04 environment (AMD Ryzen™ 9 
3950X, 32 threads, 128 GB RAM, 1 TB SSD). The codes described below are designed 
to be used in the terminal, there is no user interface to reproduce this.

There is a recommended folder structure for reproducing this. Described as:

```
#Folder structure

~/Oak-wilt
- /level2
- /level3_lansat
- /level3_sentinel
- /level4_sentinel
- /log
-- /log_sentinel
-- /log_landsat
- /param
- /temp
- /misc
-- /dem
-- /GIS
-- /grid
-- /wvdb
- /R
```

<br/>


----

#### **Projection** 

<br/>

The base for FORCE is spatial data cubes. Because of this, a common projection 
among scenes need to be used. Since the project aims to be expanded to other 
areas of USA (not just Minnesota), the 
[USA Contiguous Lambert Conformal Conic](https://epsg.io/102004) 
projection was selected. This projection can be validaded using gdal following:

```
gdalsrsinfo -v PROJCS["USA_Contiguous_Lambert_Conformal_Conic",GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137,298.257222101]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Lambert_Conformal_Conic"],PARAMETER["False_Easting",0],PARAMETER["False_Northing",0],PARAMETER["Central_Meridian",-96],PARAMETER["Standard_Parallel_1",33],PARAMETER["Standard_Parallel_2",45],PARAMETER["Latitude_Of_Origin",39],UNIT["Meter",1]]

```

***All the scenes that were created in this project have this projection!***

<br/>

----

#### **Landsat-8 archive processing**

<br/>

##### LEVEL-2 Landsat-8 archive processing
```
force-level2 /Oak-wilt/param/L2_oak-wilt_landsat.prm
```

##### Report of Landsat-8 LEVEL-2
```
force-level2-report /Oak-wilt/log_landsat
```

##### LEVEL-3 monthly Landsat-8 NIR for coregistration
```
force-higher-level /Oak-wilt/param/L3_landsat-tsa.prm
```

<br/>

----

#### **Sentinel-2 archive processing**

<br/>

##### LEVEL-2 Sentinel-2 archive

```
force-level2 /Oak-wilt/param/L2_oak-wilt_sen2.prm
```

##### Report of Sentinel-2 LEVEL-2

```
force-level2-report /Oak-wilt/log_sen2
```

##### LEVEL-3 time series Sentinel-2

```
force-higher-level /Oak-wilt/param/L3_sentinel-tsa.prm
```

<br/>

----

#### **Miscellaneous steps for targeting regions and efficient processing**

<br/>

##### Create grid of data cube to see the distribution of tiles

```
force-tabulate-grid -b 42.491920000,49.38435799999,-97.2290390000,-86.7816795053 -f shp /Oak-wilt/level2
```

##### Create a mask

```
force-cube /Oak-wilt/misc/mask/aos.gpkg /Oak-wilt/misc/mask rasterize 10
force-mosaic /Oak-wilt/misc/mask
```

##### Create mosaic

```
force-mosaic /Oak-wilt/level3
```
