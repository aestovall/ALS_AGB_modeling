#load necessary packages
library(maptools)
library(raster)
library(rgdal)
library(dplyr)
library(stringr)
library(viridis)
library(lidR)
library(rlas)
library(future)
library(parallel)


#set directory for las files
las_dir<-"data/lidar/CMS_LiDAR_Point_Cloud_Zambezi_1521/data"
las_files<-list.files(las_dir, pattern = ".laz", full.names = TRUE)

#what is the projection?
proj.utm<-crs(readOGR(list.files("data/lidar/",pattern="shp", full.names = TRUE)))

#create a las catalog and set projection
ctg <- catalog(las_files)
crs(ctg)<-proj.utm
plot(ctg, mapview = TRUE)

#set output file format
opt_output_files(ctg) <- paste(las_dir,"/normalized/{ORIGINALFILENAME}_NORM",sep = "")

#set up for running in parallel
cores<-detectCores()
set_lidr_threads(cores-2)
plan(multisession, workers=cores-2)

#Create las normalizing function
myfun = function(cluster, ...)
{
  las = readLAS(cluster)
  crs(las)<-proj.utm
  if (is.empty(las)) return(NULL)
  
  #adjust "res" depending on lidar quality. 
  #Lower resolution lidar required the DEM resolution to be higher.
  DTM<-grid_terrain(las, res = 5, knnidw(rmax=100))
  
  #normalize lidar
  las_norm <- las-DTM
  return(las_norm)
}

#apply function to entire las catalog
catalog_apply(ctg, myfun)

