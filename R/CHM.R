library(lidR)
las_dir<-"PointCloud/normalized"
ctg <- catalog(las_dir)

# ctg <- catalog(las_files)
crs(ctg)<-proj.utm
plot(ctg, mapview = TRUE)

opt_output_files(ctg) <- paste(las_dir,"/chm/{ORIGINALFILENAME}_CHM",sep = "")

las

grid_canopy(ctg, res=1, pitfree(c(0,2,5,10,15), c(0, 1.5)))
# plot(chm)

grid_canopy(ctg, res=1, pitfree(c(0,2,5,10,15), c(0, 1.5)))
