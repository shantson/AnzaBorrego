

library(raster)
install.packages("plotly")
library(plotly)

mask_anza=raster("/Users/stijnhantson/Documents/projects/anza_borrego/data/mask_anza.tif")
mask_sh = shapefile("/Users/stijnhantson/Documents/projects/anza_borrego/extra_mask.shp")
mean_p = raster("/Users/stijnhantson/Documents/projects/anza_borrego/results/mean_precip_30m.tif")
d_dem = raster("/Users/stijnhantson/Documents/projects/anza_borrego/DEM_anza/anza_dem_proj.tif")
left = shapefile("/Users/stijnhantson/Documents/projects/anza_borrego/temp/test_l.shp")

TA = crs(mask_anza)

left = spTransform(left,TA)

mask_sh=spTransform(mask_sh,TA)
mask_anza[mask_anza>0]=0
mask_anza[is.na(mask_anza)]=1
mask_anza = mask(mask_anza,mask_sh)
mask_anza[mask_anza==0]= NA
mask_anza[ is.na(mean_p)] = NA


plot(d_dem)

dd = d_dem
dd[] = NA

dd[d_dem >= 700 & d_dem < 1500] = 1
dd=dd/mask_anza

dd1=dd
dd1[] = NA
dd1[d_dem < 700] = 1
dd1 = mask(dd1,left)
dd1=dd1/mask_anza

mask_landsat = dd
mask_landsat[] = 1
list_rec=list.files("/Users/stijnhantson/Documents/projects/anza_borrego/annual_june_sept/",pattern = "summer_ndvi.tif$", recursive = TRUE, full.names=T)
list_rec_temp=list.files("/Users/stijnhantson/Documents/projects/anza_borrego/temp/annual_oct/",pattern = ".tif$", recursive = TRUE, full.names=T)
list_rec_prec=list.files("/Users/stijnhantson/Documents/projects/anza_borrego/precip/annual_oct/",pattern = ".tif$", recursive = TRUE, full.names=T)
list_rec_prec1=list.files("/Users/stijnhantson/Documents/projects/anza_borrego/precip/lag_3year/",pattern = ".tif$", recursive = TRUE, full.names=T)
list_rec_prec2=list.files("/Users/stijnhantson/Documents/projects/anza_borrego/precip/lag_4year/",pattern = ".tif$", recursive = TRUE, full.names=T)


desert = 0
mountain = 0
desert_t = 0
mountain_t = 0
desert_p = 0
mountain_p = 0
desert_p1 = 0
mountain_p1 = 0
desert_p2 = 0
mountain_p2= 0
temp = 0
prec = 0
for (ps in 1:length(list_rec)){
  ndvi = raster(list_rec[ps])
  tem = raster(list_rec_temp[ps+2])
  pre = raster(list_rec_prec[ps+2])
  pre1 = raster(list_rec_prec1[ps+3])
  pre2 = raster(list_rec_prec2[ps+3])
  tem = projectRaster(tem, ndvi, method="bilinear")
  pre = projectRaster(pre, ndvi, method="bilinear")
  pre1 = projectRaster(pre1, ndvi, method="bilinear")
  pre2 = projectRaster(pre2, ndvi, method="bilinear")
  
  tr1=mask(ndvi,dd)
  tr2=mask(ndvi,dd1)
  
  tr3=mask(tem,dd)
  tr4=mask(tem,dd1)  
  
  tr5=mask(pre,dd)
  tr6=mask(pre,dd1) 
  
  tr7=mask(pre1,dd)
  tr8=mask(pre1,dd1) 
  
 tr9=mask(pre2,dd)
  tr10=mask(pre2,dd1) 
  
  desert[ps] = mean(getValues(tr2),na.rm=T)
  mountain[ps] = mean(getValues(tr1),na.rm=T)
  
  desert_t[ps] = mean(getValues(tr4),na.rm=T)
  mountain_t[ps] = mean(getValues(tr3),na.rm=T)
  
  desert_p[ps] = mean(getValues(tr6),na.rm=T)
  mountain_p[ps] = mean(getValues(tr5),na.rm=T)
  
  desert_p1[ps] = mean(getValues(tr8),na.rm=T)
  mountain_p1[ps] = mean(getValues(tr7),na.rm=T)
  
  desert_p2[ps] = mean(getValues(tr10),na.rm=T)
  mountain_p2[ps] = mean(getValues(tr9),na.rm=T)
}

tim = c(1984:2017)

par(mfrow=c(3,2),pty="m")
par(mar = c(3.0, .5, 2, 0.5))
par(mgp = c(2.0, .5, 0))
par(oma = c(3, 0, 1, 0))

par(mai=c(0,0.5,0,0.1))
plot(tim, (desert/10000), xlab="", ylab="NDVI", type="l", ylim=c(0.04,0.1),xaxt='n',cex.lab =1.3)
axis(side=1, labels=FALSE)
plot(tim, (mountain/10000), xlab="", ylab="NDVI", type="l", ylim=c(0.2,0.35),xaxt='n',cex.lab =1.3)
axis(side=1, labels=FALSE)
plot(tim, (desert_p), xlab="", ylab="Precipitation (mm)", ylim=c(0,270),type="l",xaxt='n',cex.lab =1.3)
axis(side=1, labels=FALSE)
plot(tim, (mountain_p), xlab="", ylab="Precipitation (mm)",ylim=c(0,880), type="l",xaxt='n',cex.lab =1.3)
axis(side=1, labels=FALSE)
plot(tim, (desert_t/10), xlab="Year", ylab="Temperature (deg. C)", ylim=c(25,28.5),type="l",cex.lab =1.3)
mtext("year",side=1,line=1.8)
plot(tim, (mountain_t/10), xlab="Year", ylab="Temperature (deg. C)",ylim=c(17.5,21), type="l",cex.lab =1.3)
mtext("year",side=1,line=1.8)


dev.off()


plot(desert_p,desert,ylim=c(0,1000))
points(desert_p[1:16],desert[1:16], col="red")
abline(lm(desert[1:16]~desert_p[1:16]), col="red")
abline(lm(desert[17:34]~desert_p[17:34]))

plot(mountain_p,mountain,ylim=c(0,4000))
points(mountain_p[1:16],mountain[1:16], col="red")
abline(lm(mountain[1:16]~mountain_p[1:16]), col="red")
abline(lm(mountain[17:34]~mountain_p[17:34]))

summary(lm(desert~desert_p))
summary(lm(desert~desert_p+desert_t))

summary(lm(desert~desert_p))
summary(lm(desert~desert_p+desert_t))


trs=lm(desert~desert_p+test[1:34]+desert_t)
plot(tim,resid(trs))
abline(lm(resid(trs)~tim))

plot(resid(trs),desert_p)
plot(desert_p,resid(trs))
abline(lm(resid(trs)~desert_p))


trs=lm(mountain~mountain_p+test1[1:34]+mountain_t)
plot(tim,resid(trs))
abline(lm(resid(trs)~tim))

plot(resid(trs),mountain_p)
plot(mountain_p,resid(trs))
abline(lm(resid(trs)~mountain_p))


test=0
test1=0
test[1]=355.3016
test1[1]=879.9149
for (i in 1:34){
  
  test[i+1]=desert_p[i]
  test1[i+1]=mountain_p[i]
}
