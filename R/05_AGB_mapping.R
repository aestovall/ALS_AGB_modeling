las_dir<-"data/lidar/CMS_LiDAR_Point_Cloud_Zambezi_1521/data"
las_dir<-paste0(las_dir,"/normalized")
las_files<-list.files(las_dir, pattern = ".las", full.names = TRUE)

# las_files<-las_files[93:94]
proj.utm<-crs(readOGR(list.files(pattern="shp")))
# epsg <- make_EPSG()
# proj<-epsg %>% filter(code == 2251)


ctg <- catalog(las_files)
crs(ctg)<-proj.utm
plot(ctg, mapview = TRUE)
plot(plots.sp.t, add=TRUE)

opt_output_files(ctg) <- paste(las_dir,"/AGB_SubPlot_model/{ORIGINALFILENAME}_AGB",sep = "")

library(parallel)
library(future)

cores<-detectCores()
set_lidr_threads(cores-2)
plan(multisession, workers=cores-2)

myfun = function(cluster, ...)
{
  las = readLAS(cluster)
  crs(las)<-proj.utm
  if (is.empty(las)) return(NULL)
  
  # zmax+zq30+zq50+zq70+zpcum9
  
  las.metrics<-grid_metrics(las, res = 7, .stdmetrics_z)
  las.metrics<-dropLayer(las.metrics, 6)
  # plot(las.metrics)
  # names(las.metrics)
  # plot(setMinMax(predict(las.metrics, mlr.m)))
  return(setMinMax(predict(las.metrics, mlr.m)))
}

catalog_apply(ctg, myfun)

# normalize_height(ctg, res = 5, algorithm=knnidw())

agb.map<-do.call(merge,
                 lapply(
                   list.files( paste0(las_dir, "/AGB_SubPlot_nls_model"), 
                               full.names=TRUE, pattern="tif"), 
                   raster)
)

writeRaster(agb.map,"output/agb_map.tif")


