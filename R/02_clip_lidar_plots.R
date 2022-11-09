#Read plot data. This must be modified depending on input data format.
plots<-read.csv("data/field/Zambezi_2013_Subplot_Data.csv")

#Make a spatial object
plots.sp<-SpatialPointsDataFrame(coords=cbind(plots$Longitude,plots$Latitude)[!is.na(plots$Longitude),1:2],
                                 data=plots[!is.na(plots$Longitude),1:32],
                                 proj4string = crs(raster()))

#Reproject plots to lat/lon
plots.sp.t<-sp::spTransform(plots.sp, proj.utm)

#location of normalized las data
las_dir<-paste0(las_dir,"/normalized")

#create another las catalog with normalized data
ctg <- catalog(las_dir)
crs(ctg)<-proj.utm

#Do plots line up with las tiles?
plot(ctg)
plot(plots.sp.t, add=TRUE)

#Output file naming scheme
opt_output_files(ctg) <- paste(las_dir,"/plots/{ID}",sep = "")

#use the correct plot clipping function depending on plot geometry. This is most often circular, but could also be a rectangle and polygon geometry.

clip_circle(ctg, plots.sp.t@coords[,1], plots.sp.t@coords[,2], radius = 7)

# clip_rectangle(ctg, plots.sp.t@coords[,1]-36, plots.sp.t@coords[,2]-36,
#                plots.sp.t@coords[,1]+36, plots.sp.t@coords[,2]+36)
