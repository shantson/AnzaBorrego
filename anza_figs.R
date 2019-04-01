

library(raster)
library(LSD)

mask_anza=raster("/Users/stijnhantson/Documents/projects/anza_borrego/data/mask_anza.tif")
mask_sh = shapefile("/Users/stijnhantson/Documents/projects/anza_borrego/extra_mask.shp")

mean_ndvi=raster("/Users/stijnhantson/Documents/projects/anza_borrego/results/mean_NDVI_june-sep.tif")

mean_p = ("/Users/stijnhantson/Documents/projects/anza_borrego/results/mean_precip_30m.tif")
dem=raster("/Users/stijnhantson/Documents/projects/anza_borrego/DEM_anza/anza_dem_proj.tif")
clim = raster("/Users/stijnhantson/Documents/projects/anza_borrego/precip/perc_precip_change_30m.tif")
temp = raster("/Users/stijnhantson/Documents/projects/anza_borrego/results/trend_temperature_30m.tif")
mean_temp = raster("/Users/stijnhantson/Documents/projects/anza_borrego/results/mean_temperature_30m.tif")
spring = raster("/Users/stijnhantson/Documents/projects/anza_borrego/results/realtive_NDVI_trend_jan_march.tif")
summer = raster("/Users/stijnhantson/Documents/projects/anza_borrego/results/realtive_NDVI_trend_april_june.tif")
autumn = raster("/Users/stijnhantson/Documents/projects/anza_borrego/results/realtive_NDVI_trend_july_sept.tif")
winter = raster("/Users/stijnhantson/Documents/projects/anza_borrego/results/realtive_NDVI_trend_oct_dec.tif")

d_fire = "/Users/stijnhantson/Documents/data/FRAP/firep16_1_last fire.tif"
fire=raster(d_fire)

ndvi_precip = raster("/Users/stijnhantson/Documents/projects/anza_borrego/results/summer_ndvi_precip_rsquare.tif")
ndvi_all = raster("/Users/stijnhantson/Documents/projects/anza_borrego/results/winter_precip_summer_ndvi_precip_precip1lag3temp_rsquare2.tif")

predict_precip = raster("/Users/stijnhantson/Documents/projects/anza_borrego/results/winter_precip_summer_ndvi_predict_slope.tif")
predict_all = raster("/Users/stijnhantson/Documents/projects/anza_borrego/results/winter_precip_summer_ndvi_precip_precip1lag3temp_predict_slope_relative.tif")

TA = crs(mask_anza)

mask_sh=spTransform(mask_sh,TA)
mask_anza[mask_anza>0]=0
mask_anza[is.na(mask_anza)]=1
mask_anza = mask(mask_anza,mask_sh)
mask_anza[mask_anza==0]= NA
mask_anza[ is.na(clim)] = NA
summer=summer/mask_anza
plot(summer)
summer[summer>3]=3
summer[summer< -3]=-3
col1=colorRampPalette(c("red","white","blue"))
plot(summer, col=col1(20), xlim=c(500000,635000))



reg1=stack(dem,summer)
v<- data.frame(values(reg1))
names(v)<-c("L1","L2")
sub<-v[sample(nrow(v), 50000),]
heatscatter(sub$L1,sub$L2,ylim=c(-3,2),xlim=c(-60,2000),xlab = "NDVI change (%/yr.)", ylab = "elevation (m)")


reg1=stack(clim,summer)
v<- data.frame(values(reg1))
names(v)<-c("L1","L2")
sub<-v[sample(nrow(v), 300000),]
heatscatter(sub$L1,sub$L2,ylim=c(-3,2),ylab = "NDVI change (%/yr.)", xlab = "precipitation (mm)")

col1=colorRampPalette(c("red","white","blue"))
plot(clim, col=col1(20), xlim=c(500000,635000),zlim=c(-3,3))

#temp = (temp/mean_temp)*100
col1=colorRampPalette(c("blue","white","red"))
plot(temp, col=col1(20), xlim=c(500000,635000),zlim=c(-0.08,0.08))

spring=spring/mask_anza
col1=colorRampPalette(c("red","white","blue"))
plot(spring, col=col1(20), xlim=c(500000,635000),zlim=c(-3,3))

summer=summer/mask_anza
col1=colorRampPalette(c("red","white","blue"))
plot(summer, col=col1(20), xlim=c(500000,635000),zlim=c(-3,3))

autumn=autumn/mask_anza
col1=colorRampPalette(c("red","white","blue"))
plot(autumn, col=col1(20), xlim=c(500000,635000),zlim=c(-3,3))

winter=winter/mask_anza
col1=colorRampPalette(c("red","white","blue"))
plot(winter, col=col1(20), xlim=c(500000,635000),zlim=c(-3,3))

col2=colorRampPalette(c("white","darkgreen"))

ndvi_precip = ndvi_precip/mask_anza
plot(ndvi_precip, xlim=c(500000,635000), zlim=c(0,0.8))

ndvi_all = ndvi_all/mask_anza
plot(ndvi_all, xlim=c(500000,635000), zlim=c(0,0.8))

predict_precip=predict_precip/mask_anza
predict_precip=(predict_precip/mean_ndvi)*100
col1=colorRampPalette(c("red","white","blue"))
plot(predict_precip, col=col1(20), xlim=c(500000,635000),zlim=c(-1,1))

predict_all=predict_all/mask_anza
col1=colorRampPalette(c("red","white","blue"))
plot(predict_all, col=col1(20), xlim=c(500000,635000),zlim=c(-3,3))

fire=fire/mask_anza
col1=colorRampPalette(c("yellow","red"))
plot(fire, col=col1(20), xlim=c(500000,635000))

reg1=stack(clim,ndvi_precip)
v<- data.frame(values(reg1))
names(v)<-c("L1","L2")
sub<-v[sample(nrow(v), 300000),]
heatscatter(sub$L1,sub$L2,ylim=c(-3,2),ylab = "NDVI change (%/yr.)", xlab = "precipitation (mm)")

