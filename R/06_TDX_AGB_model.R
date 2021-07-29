# agb<-raster("PointCloud/Normalized/AGB/AGB.vrt")
tdx<-raster("data/Mozambique_GMW2016_12m_TDX.tif")

r_ls<-lapply(list.files("PointCloud/normalized/AGB_SubPlot_nls_model", pattern="AGB.tif", recursive = FALSE,
                        full.names = TRUE),
             function(x) stack(x))
r_ls$fun <- mean
r_ls$na.rm <- TRUE

agb <- do.call(mosaic, r_ls)
plot(agb)


plot(extract(agb[[1]], plots.sp.t)[plots.sp.t@data$Total.AGB<1000],
     plots.sp.t@data$Total.AGB[plots.sp.t@data$Total.AGB<1000])
abline(0,1)

writeRaster(agb, "data/lidar/normalized/AGB_SubPlot_nls_model/AGB.tif", overwrite=TRUE)

agb.t<-stack("data/lidar/normalized/AGB_SubPlot_nls_model/AGB_lat_lon.tif")

agb.t[agb.t>1000]<-1000
agb.t[agb.t<0]<-0
plot(agb.t, col=viridis(250))

# agb.t<-projectRaster(agb, tdx)

tdx.c<-crop(tdx, agb.t)
agb.t<-projectRaster(agb.t, tdx.c, res=res(tdx))

tdx.c[tdx.c<0]<-NA

tdx.c
agb.t

# tdx.metrics<-stack(aggregate(tdx.c, fact=res(agb.t)/ res(tdx.c), fun="mean"),
#       aggregate(tdx.c, fact=res(agb.t)/ res(tdx.c), fun="sd"),
#       aggregate(tdx.c, fact=res(agb.t)/ res(tdx.c), fun="max"),
#       aggregate(tdx.c, fact=res(agb.t)/ res(tdx.c), fun="median"))

tdx.metrics<-stack(tdx.metrics,
                   3*(tdx.metrics[[1]]-tdx.metrics[[4]])/tdx.metrics[[2]])
plot(tdx.metrics)
# names(tdx.metrics)<-c("TDXmean","TDXsd","TDXmax", "TDXmedian", "TDXskewness")

agb.re<-resample(agb.t, tdx.metrics)

stack(agb.t, tdx.c)

# agb.re<-resample(agb.t, tdx.c)
# 
# agb.re[is.na(tdx.metrics[[1]])]<-NA
lidar.tdx.agb<-data.frame(rasterToPoints(stack(agb.t, tdx.c)))
colnames(lidar.tdx.agb)[3:5]<-c("AGB","AGB_sd","TDX")

errors(lidar.tdx.agb$AGB)<-lidar.tdx.agb$AGB_sd

tdx.lidar.m<-lm(AGB~TDX,
           na.omit(lidar.tdx.agb[lidar.tdx.agb$AGB<850,]))
summary(tdx.lidar.m)

lidar.tdx.agb$lidar.predict<-predict(tdx.lidar.m, newdata=lidar.tdx.agb)

sqrt(mean((lidar.tdx.agb$lidar.predict.error-lidar.tdx.agb$AGB)^2, na.rm=TRUE))/mean(lidar.tdx.agb$AGB, na.rm=TRUE)

library(ggplot2)
library(errors)
library(viridis)

y1 <- set_errors(coef(tdx.lidar.m)[1], sqrt(vcov(tdx.lidar.m)[1, 1]))
y2 <- set_errors(coef(tdx.lidar.m)[2], sqrt(vcov(tdx.lidar.m)[2, 2]))
covar(y1, y2) <- vcov(tdx.lidar.m)[1, 2]

lidar.tdx.agb$lidar.predict.error<-(y1+y2*lidar.tdx.agb$TDX)

ggplot(lidar.tdx.agb, aes(TDX, AGB))+
  geom_hex(bins=300)+
  ylim(0,500)+xlim(0,30)+
  stat_smooth(formula = y~x-1, color="red")
  # geom_abline(slope=1, intercept = 0, linetype=1)

ggplot(lidar.tdx.agb, aes(lidar.predict, AGB))+
  geom_hex(bins=300)+
  ylim(0,400)+xlim(0,400)+
  stat_smooth(formula = y~x, color="red")+
  scale_fill_viridis(option="inferno")+
  geom_abline(yintercept=0, slope=1)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none")

write.csv(lidar.tdx.agb, "output/zambezi_delta_ALS_to_AGB_TDX.csv")  





