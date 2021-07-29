library(errors)

las_dir<-"data/lidar/normalized"
las_files<-list.files(las_dir, pattern = ".las", full.names = TRUE)

proj.utm<-crs(readOGR(list.files(pattern="shp")))

ctg <- catalog(las_files)
crs(ctg)<-proj.utm
opt_chunk_buffer(ctg) <- 10
plot(ctg, mapview = TRUE)
plot(plots.sp.t, add=TRUE)

opt_output_files(ctg) <- paste(las_dir,"/AGB_SubPlot_nls_model/{ORIGINALFILENAME}_AGB",sep = "")

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
  
  las.metrics<-grid_metrics(las, res = 7, .stdmetrics_z)
  las.metrics<-dropLayer(las.metrics, 6)
  

  require(errors)
  
  y1 <- set_errors(coef(nls.m)[1], sqrt(vcov(nls.m)[1, 1]))
  y2 <- set_errors(coef(nls.m)[2], sqrt(vcov(nls.m)[2, 2]))
  
  covar(y1, y2) <- vcov(nls.m)[1, 2]
  
  AGB<-setMinMax(predict(las.metrics, nls.m))

  r.e<-getValues(las.metrics[['zmax']])
  
  pred<-(y1*r.e^y2)
  
  AGB.errors<-AGB
  values(AGB.errors)<-errors(pred)
  values(AGB)<-as.numeric(pred)
  
  plot(stack(AGB,AGB.errors))
  
  return(stack(AGB,AGB.errors))
  
}

catalog_apply(ctg, myfun)
