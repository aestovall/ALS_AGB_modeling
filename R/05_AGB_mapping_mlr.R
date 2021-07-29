library(errors)

las_dir<-"data/lidar/normalized"
las_files<-list.files(las_dir, pattern = ".las", full.names = TRUE)

proj.utm<-crs(readOGR(list.files(pattern="shp")))

ctg <- catalog(las_files)
crs(ctg)<-proj.utm
opt_chunk_buffer(ctg) <- 10
plot(ctg, mapview = TRUE)
plot(plots.sp.t, add=TRUE)

opt_output_files(ctg) <- paste(las_dir,"/AGB_SubPlot_model/{ORIGINALFILENAME}_AGB",sep = "")

library(parallel)
library(future)

cores<-detectCores()
set_lidr_threads(cores-2)
plan(multisession, workers=cores-2)


#modeling using 4-variable model
myfun = function(cluster, ...)
{
  las = readLAS(cluster)
  crs(las)<-proj.utm
  if (is.empty(las)) return(NULL)
  
  # zmax+zq30+zq50+zq70+zpcum9
  
  las.metrics<-grid_metrics(las, res = 7, .stdmetrics_z)
  las.metrics<-dropLayer(las.metrics, 6)
  

  require(errors)
  
  y1 <- set_errors(coef(mlr.m)[1], sqrt(vcov(mlr.m)[1, 1]))
  y2 <- set_errors(coef(mlr.m)[2], sqrt(vcov(mlr.m)[2, 2]))
  y3 <- set_errors(coef(mlr.m)[3], sqrt(vcov(mlr.m)[3, 3]))
  y4 <- set_errors(coef(mlr.m)[4], sqrt(vcov(mlr.m)[4, 4]))
  
  covar(y1, y2) <- vcov(mlr.m)[1, 2]
  covar(y1, y3) <- vcov(mlr.m)[1, 3]
  covar(y1, y4) <- vcov(mlr.m)[1, 4]
  
  covar(y2, y3) <- vcov(mlr.m)[2, 3]
  covar(y2, y4) <- vcov(mlr.m)[2, 4]
  
  covar(y3, y4) <- vcov(mlr.m)[3, 4]
  
  AGB<-setMinMax(predict(las.metrics, mlr.m))

  
  las.metrics<-stack(lapply(names(coef(mlr.m))[-1], function(x) las.metrics[[x]]))
  
  r.e<-getValues(las.metrics)
  
  pred<-(y1 + y2*r.e[,1] + y3*r.e[,2] + y4*r.e[,3])
  
  AGB.errors<-AGB
  values(AGB.errors)<-errors(pred)
  values(AGB)<-as.numeric(pred)
  
  return(stack(AGB,AGB.errors))
  
}

catalog_apply(ctg, myfun)
