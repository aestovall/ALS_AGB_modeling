# Getting Started with LiDAR Data

## Installation of lidR

First you will need to install lidR and all dependencies. Follow the instructions on the GitHub page [here](https://github.com/r-lidar/lidR). Simply installing using the `install.packages`function. The following packages are necessary to run the code in this tutorial:
```r
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
```    

## Download LiDAR data in R

Now you'll need to download the lidar data used in this tutorial. We will be using an ALS dataset collected in the Zambezi Delta over a mangrove forest.

Download the public lidar data from [here](https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1521).

You will need to sign up/register for an EarthData account to get access to these lidar data. You can also download from this link [here]() (not guaranteed to be active).

Once you have downloaded the lidar data place the files in the `data/lidar` directory.

## Read and Setup the LAS catalog

Now we need to find our LAS files and access them for further processing. First, we read the file list. Then we make that list into a catalog, which essentially enables parallel processing and reduces edge effects in the resulting maps. LiDAR data is nearly always "tiled" - meaning the 3D data is split into small chunks (1 km x 1 km, for example). The catalog is made of these lidar tiles.

```{r}
#set directory for las files
las_dir<-"data/lidar"
las_files<-list.files(las_dir, pattern = ".las", full.names = TRUE)

#what is the projection?
proj.utm<-crs(readOGR(list.files(pattern="shp")))

#create a las catalog and set projection
ctg <- catalog(las_files)
crs(ctg)<-proj.utm
plot(ctg, mapview = TRUE)

```
<img src="lidar_cat.png">

Next we can conveniently change the naming scheme for the processed lidar files. Since we will first be normalizing the lidar point cloud by topography we use `_NORM` in our naming convention.

```{r}
#set output file format
opt_output_files(ctg) <- paste(las_dir,"/normalized/{ORIGINALFILENAME}_NORM",sep = "")
```

Now we must setup our processing to run in parallel. This will ensure we are using the CPU cores and resources that are available to us. I like to leave two cores free so the computer doesn't become super bogged down. You can adjust this with `set_lidr_threads`.

```{r}
#set up for running in parallel
cores<-detectCores()
set_lidr_threads(cores-2)
plan(multisession, workers=cores-2)
```

## Create a custom function for normalizing the LiDAR data

Finally, we will create a function to normalize the las data.

```{r}
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
```
A new folder names `normalized` will be created in the same folder with the las data containing the same lidar data, but now normalized to the topography.

<img src="lidar_preview_int.png">

## Clip the forest inventory plots out of the LiDAR data

Now that we have flattened our lidar data we can clip out our plot locations, allowing us to later derive point cloud statistics.

First we will read in the plot level data and make it into a spatial object. These data describe the location of data collection (plot center) and a number of useful field-measured variables. We are most interested in biomass for this tutorial, but field data may also include other ecological variables (e.g. animal observations) that may be linked to the lidar data.

```{r}
#Read plot data. This must be modified depending on input data format.
plots<-read.csv("data/field/Zambezi_2013_Subplot_Data.csv")

#Make a spatial object
plots.sp<-SpatialPointsDataFrame(coords=cbind(plots$Longitude,plots$Latitude)[!is.na(plots$Longitude),1:2],
                                 data=plots[!is.na(plots$Longitude),1:32],
                                 proj4string = crs(raster()))
```

We must reproject the plot locations into the UTM coordinate system (same as the lidar).

```{r}
#Reproject plots to lat/lon
plots.sp.t<-sp::spTransform(plots.sp, proj.utm)
```

Again, we will create a las catalog and complete the setup as in the previous section.

```{r}
#location of normalized las data
las_dir<-paste0(las_dir,"/normalized")

#create another las catalog with normalized data
ctg <- catalog(las_dir)
crs(ctg)<-proj.utm

#Do plots line up with las tiles?
plot(ctg)
plot(plots.sp.t, add=TRUE)
```
Here we can see where the plot locations are relative to the lidar data.

Finally, we will use the `clip_circle` or `clip_rectangle` function to create the plot-level lidar point clouds.

```{r}
#Output file naming scheme
opt_output_files(ctg) <- paste(las_dir,"/plots/{ID}",sep = "")

#use the correct plot clipping function depending on plot geometry. This is most often circular, but could also be a rectangle and polygon geometry.

clip_circle(ctg, plots.sp.t@coords[,1], plots.sp.t@coords[,2], radius = 7)

# clip_rectangle(ctg, plots.sp.t@coords[,1]-36, plots.sp.t@coords[,2]-36,
#                plots.sp.t@coords[,1]+36, plots.sp.t@coords[,2]+36)
```
Let's visualize how the plots align with the lidar data.

<img src="lidar_cat_plots.png">

Below you can see the lidar data clipped to the plot areas. In this case we have a center plot and 4 other surrounding plots.

<img src="lidar_preview_int_plot.png">

