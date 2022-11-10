library(lidR)
las_dir<-"data/lidar/CMS_LiDAR_Point_Cloud_Zambezi_1521/data"
las_dir<-paste0(las_dir,"/normalized")
ctg <- catalog(las_dir)

# ctg <- catalog(las_files)
crs(ctg)<-proj.utm
plot(ctg, mapview = TRUE)

opt_output_files(ctg) <- paste(las_dir,"/chm/{ORIGINALFILENAME}_CHM",sep = "")

grid_canopy(ctg, res=5, pitfree(c(0,2,5,10,15), c(0, 1.5)))
# plot(chm)

chm<-raster(list.files(paste0(las_dir,"/chm"), pattern = ".vrt", full.names=TRUE))
chm[chm>=60]<-NA



