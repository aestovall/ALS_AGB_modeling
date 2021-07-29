# Get plot-level LiDAR metrics

#Set lidar plot directory
las_dir<-"data/lidar/normalized/plots"
las_files<-list.files(las_dir, pattern = "las")

#Extract lidar metrics from plot list
metrics_ls<-lapply(1:length(las_files), function(x){
  i<-x
  
  #read plot LAS file
  las<-readLAS(list.files(las_dir, pattern = "las", full.names = TRUE)[i])
  #set projection
  crs(las)<-proj.utm
  
  #get LiDAR metrics
  return(
    
    data.frame(ID = as.character(gsub(".las", "", las_files[i])), 
                              cloud_metrics(las, stdmetrics(X,Y,Z,Intensity,ReturnNumber,Classification)),
                              stringsAsFactors = FALSE)
    )
})

metrics_all<-do.call(rbind,metrics_ls)


#Match plot ID names
metrics_all$Plot.Subplot<-as.character(plots.sp.t@data$Plot.Subplot)[as.numeric(metrics_all$ID)]

#merge lidar metrics to plot data
metrics_all.m<-merge(metrics_all, na.omit(data.frame(Plot=plots$Plot,
                                                     Plot.Subplot=plots$Plot.Subplot,
                                                     AGB=plots$Total.AGB,
                                             height.mean=plots$mean_height,
                                             height.max=plots$H100_field)), by="Plot.Subplot")

#what is the noData value? Exclude those plot that do not overlap with lidar data
metrics_all.m<-metrics_all.m[metrics_all.m$AGB<1000,]

write.csv(metrics_all.m, "output/metrics_all_m.csv", row.names = FALSE)

