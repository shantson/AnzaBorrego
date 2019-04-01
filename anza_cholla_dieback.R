

library(raster)
library(rgdal)
library(maptools)
library(rgeos)
P4S.latlon <- CRS("+proj=longlat +datum=WGS84")

da=raster("/Users/stijnhantson/Documents/projects/anza_borrego/results/february_trend_ndvi.tif")


y = c(33.65688,33.65446,33.65173,33.65688,33.65446,33.65173)
x = c(-116.37411,-116.37411,-116.37411,-116.37154,-116.37154,-116.37154)

y = c(33.64146,33.63823,33.63650,33.63192,33.63067,33.63093)
x = c(-116.39708,-116.3932,-116.40148,-116.37928,-116.37782,-116.37587)

dr = cbind(x,y)
mdf=as.data.frame(dr)
xy <- mdf[,c(1,2)]

spdf <- SpatialPointsDataFrame(coords = xy, data= mdf,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


spdf2 <- spTransform(spdf, CRS("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))


imgs = list.files("/Users/stijnhantson/Documents/projects/anza_borrego/annual/",recursive = TRUE, full.names=T, pattern = ".tif$")
dr=stack(imgs)
dr1=brick(dr)
#dr1=raster("/Users/stijnhantson/Documents/projects/anza_borrego/DEM_anza/anza_dem_proj.tif")
ts = extract(dr1,spdf2)
write.table(ts,"/Users/stijnhantson/Documents/projects/anza_borrego/cholla_mort2.txt")


plot(ts)
shapefile(spdf2, "/Users/stijnhantson/Documents/projects/anza_borrego/cholla_dieback.shp", overwrite=TRUE)

dr=stack(imgs[12:15])
mean1998=mean(dr)

mean2005=raster(imgs[22])

dr=stack(imgs[26:28])
mean2010=mean(dr)

dif1998_2005=((mean2005-mean1998)/mean1998)*100
dif2005_2010=((mean2010-mean2005)/mean1998)*100
dif1998_2010=((mean2010-mean1998)/mean1998)*100

plot(dif1998_2005)
plot(dif2005_2010)
plot(dif1998_2010)

writeRaster(dif1998_2005,"/Users/stijnhantson/Documents/projects/anza_borrego/results/dif1998_2005.tif")
writeRaster(dif2005_2010,"/Users/stijnhantson/Documents/projects/anza_borrego/results/dif2005_2010.tif")
writeRaster(dif1998_2010,"/Users/stijnhantson/Documents/projects/anza_borrego/results/dif1998_2010.tif")


