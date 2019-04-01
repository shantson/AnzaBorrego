library(raster)
library(spdep)
d_ndvi_trend = "/Users/stijnhantson/Documents/projects/anza_borrego/results/relative_summer_change_ndvi.tif"
d_ndvi = "/Users/stijnhantson/Documents/projects/anza_borrego/results/summer_mean_ndvi.tif"
d_precip_trend = "/Users/stijnhantson/Documents/projects/anza_borrego/precip/perc_precip_change_30m.tif"
d_precip = "/Users/stijnhantson/Documents/projects/anza_borrego/results/mean_precip_30m.tif"
d_slope = "/Users/stijnhantson/Documents/projects/anza_borrego/DEM_anza/slope_30m_anza.tif"
d_dem = "/Users/stijnhantson/Documents/projects/anza_borrego/DEM_anza/anza_dem_proj.tif"
d_aspect = "/Users/stijnhantson/Documents/projects/anza_borrego/DEM_anza/cosaspect_30m.tif"
d_fire = "/Users/stijnhantson/Documents/data/FRAP/firep16_1_last fire.tif"
d_veg = '/Users/stijnhantson/Documents/data/CAL_VEG/veg_anza2.tif'
d_temp_trend = "/Users/stijnhantson/Documents/projects/anza_borrego/results/trend_temperature_30m.tif"
d_temp = "/Users/stijnhantson/Documents/projects/anza_borrego/results/mean_temperature_30m.tif"

ndvi_trend=raster(d_ndvi_trend)
ndvi=raster(d_ndvi)
precip_trend=raster(d_precip_trend)
precip=raster(d_precip)
slope=raster(d_slope)
dem=raster(d_dem)
aspect=raster(d_aspect)
fire=raster(d_fire)
veg=raster(d_veg)
temp=raster(d_temp)
temp_trend=raster(d_temp_trend)

fire[is.na(fire)] = 1949
aspect[slope<5] = 0

mask1=raster("/Users/stijnhantson/Documents/projects/anza_borrego/data/mask_anza.tif")
plot(mask1)
mask1[mask1>0]=0
mask1[is.na(mask1)]=1

lat <- lon <- aspect
xy <- coordinates(aspect)
lon[] <- xy[, 1]
lat[] <- xy[, 2]

r=brick(lon,lat,ndvi_trend,ndvi,precip_trend,precip,slope,dem,aspect,fire,temp, temp_trend)
mask=veg
mask[mask>0]=1
mask[ndvi_trend>5]=NA
mask[ndvi_trend< -5]=NA
mask[ndvi<1]=NA
mask[mask1==0]=NA
mask[mask==0]=NA
r=r/mask
names(r) <- c("lon","lat","ndvi_trend","ndvi","precip_trend","precip","slope","dem","aspect","fire","temp","temp_trend")

r.samp <- sampleRandom(r, size=50000, na.rm=TRUE, sp=FALSE, asRaster=FALSE) 
r.samp=as.data.frame(r.samp)
summary(r.samp)
#GAM regression
require(mgcv)
require(gam)
test.gam <- gam(ndvi_trend ~ s(lon,lat) + s(precip_trend)  + s(temp) + s(aspect) + s(fire) , data=r.samp)
#test.gam <- gam(ndvi_trend ~ s(ndvi), data=r.samp)
par(mfrow=c(1,1))
#plot(test.gam, se=T,resid=T)
plot(test.gam, se = TRUE)
summary(test.gam)

test.gam <- gam(ndvi_trend ~ s(precip_trend, k=4) + s(temp_trend, k=7) + s(fire,k=6) + s(precip, k=7) , data=r.samp)
#test.gam <- gam(ndvi_trend ~ s(ndvi), data=r.samp)
par(mfrow=c(1,1))
#plot(test.gam, se=T,resid=T)
plot(test.gam, se = TRUE)

resi=residuals(test.gam)
W<-bdiag(r.samp$lon,r.samp$lat)
lag<-lagsarlm(r.samp$ndvi_trend ~ 1,listw = mat2listw(W/rowSums(W)),zero.policy = T)



b_pop <- gam(log_corr_fire ~ s(log_pop_dens),sp=test.gam$sp[2])
b0 <- gam(log_corr_fire~1, data=test)
#variance explained:
100-deviance(test.gam)/deviance(b0)

#variance explained by pop dens:
(deviance(b_pop)-deviance(test.gam))/deviance(b0)

anova(test.gam)


# check the output of the car model in order to see how big rho is (should be around 0.8-0.9 and highly significant)
summary(lag)

######## prepare vegetation data

veg=raster("/Users/stijnhantson/Documents/data/CAL_VEG/gaplf2011lc_v30_CA/gaplf2011lc_v30_ca.tif")

imp<-crop(imp,ext_west)

dree1=raster("/Users/stijnhantson/Documents/projects/anza_borrego/DEM_anza/anza_dem_proj.tif")
sr=crs(dree1)
projected_raster <- projectRaster(veg, crs = sr, method="ngb",res=30)
> projected_raster[projected_raster == 577] <- NA
> projected_raster[projected_raster == 578] <- NA
> projected_raster[projected_raster == 579] <- NA
> projected_raster[test == 580] <- NA
> projected_raster[test == 583] <- NA
> projected_raster[test == 584] <- NA
> projected_raster[test == 581] <- NA
> projected_raster[test == 555] <- NA
> projected_raster[test == 556] <- NA
> projected_raster[test == 557] <- NA


writeRaster(projected_raster,'/Users/stijnhantson/Documents/data/CAL_VEG/veg_anza2.tif', overwrite=T)

projected_raster