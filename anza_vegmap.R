install.packages('e1071', dependencies=TRUE)

library("raster")
library("cluster")
library("randomForest")
library("RStoolbox")
library("rgdal")
library("plyr")
library("sf")

list_rec=list.files("/Volumes/remote_data/monthly/",pattern = ".tif$", recursive = TRUE, full.names=T)

lm_fun = function(x) {if (all(is.na(x))){NA} else {m = lm(x ~ tim); summary(m)$coefficients[2] }} # slope

precip_s1=raster::stack(list_rec[286:405])
#precip_s3=raster::brick(precip_s1)
nr_timestep=length(list_rec[286:405])
tim=(1:nr_timestep)


ext=extent(559995, 572225, 3697345, 3734445)
#precip_s1=crop(precip_s1, ext, filename="")


mean_ndvi=mean(precip_s1, na.rm=TRUE)

Q05 <- calc(precip_s1, fun = function(x) {quantile(x,probs = .05,na.rm=TRUE)} )
Q95 <- calc(precip_s1, fun = function(x) {quantile(x,probs = .95,na.rm=TRUE)} )
Q50 <- calc(precip_s1, fun = function(x) {quantile(x,probs = .5,na.rm=TRUE)} )
meanminQ50 = mean_ndvi-Q50
Q95minQ05 = Q95-Q05



temp=286+11
precip_s1=raster::stack(list_rec[286:temp])
tim=(1:12)
#precip_s1=crop(precip_s1, ext, filename="")
median <- calc(precip_s1, fun = function(x) {quantile(x,probs = .5,na.rm=TRUE)} )
for (i in 1:9){
  temp=temp+12
  temp1=temp-11
  precip_s1=raster::stack(list_rec[temp1:temp])
  #precip_s1=crop(precip_s1, ext, filename="")
  median1 <- calc(precip_s1, fun = function(x) {quantile(x,probs = .5,na.rm=TRUE)} )
  median=addLayer(median, median1)
}

sd_annual <- calc(median, fun = sd)

#plot(Q10, zlim=c(0,6000))
#plot(Q90, zlim=c(0,6000))
#plot(mean_ndvi, zlim=c(0,6000))

writeRaster(Q05,"/Users/stijnhantson/Documents/projects/anza_borrego/veg_mapping/Q05.tif",overwrite=TRUE)
writeRaster(Q95,"/Users/stijnhantson/Documents/projects/anza_borrego/veg_mapping/Q95.tif",overwrite=TRUE)
writeRaster(Q50,"/Users/stijnhantson/Documents/projects/anza_borrego/veg_mapping/Q50.tif",overwrite=TRUE)
writeRaster(sd_annual,"/Users/stijnhantson/Documents/projects/anza_borrego/veg_mapping/sd_annual.tif",overwrite=TRUE)
writeRaster(Q95minQ05,"/Users/stijnhantson/Documents/projects/anza_borrego/veg_mapping/Q95minQ05.tif",overwrite=TRUE)
writeRaster(meanminQ50,"/Users/stijnhantson/Documents/projects/anza_borrego/veg_mapping/meanminQ50.tif",overwrite=TRUE)

Q05 = raster("/Users/stijnhantson/Documents/projects/anza_borrego/veg_mapping/Q05.tif")
Q95 = raster("/Users/stijnhantson/Documents/projects/anza_borrego/veg_mapping/Q95.tif")
Q50 = raster("/Users/stijnhantson/Documents/projects/anza_borrego/veg_mapping/Q50.tif")
sd_annual = raster("/Users/stijnhantson/Documents/projects/anza_borrego/veg_mapping/sd_annual.tif")
Q95minQ05 = raster("/Users/stijnhantson/Documents/projects/anza_borrego/veg_mapping/Q95minQ05.tif")
meanminQ50 = raster("/Users/stijnhantson/Documents/projects/anza_borrego/veg_mapping/meanminQ50.tif")

indic=stack(Q05,Q95,Q50,sd_annual,Q95minQ05,meanminQ50)
v <- getValues(indic)
i <- which(!is.na(v))
v <- na.omit(v)
vx<-v[sample(nrow(v), 10000),]
rf = randomForest(vx)
rf_prox <- randomForest(vx,ntree = 1000, proximity = TRUE)$proximity

E_rf <- kmeans(rf_prox, 100, iter.max = 200, nstart = 10)
rf <- randomForest(vx,as.factor(E_rf$cluster),ntree = 500)
rf_raster<- predict(indic,rf)
plot(rf_raster)

writeRaster(rf_raster,"F:/random_forest_class2.tif",overwrite=TRUE)

te = kmeans(indic[],25,iter.max = 60,nstart = 25,algorithm = "Lloyd", na.omit = T)
result = raster(indic[[1]])
result <- setValues(result, te$cluster)
gc()


######## supervised clasification

veg_point=readOGR("/Users/stijnhantson/Documents/projects/anza_borrego/data/vegmap_1998/anzagps/anzagps.shp")
ref=readOGR("/Users/stijnhantson/Documents/projects/anza_borrego/data/keeler_wolf_map.shp")
#crs(veg_point)=crs(ref)
veg_point=spTransform(veg_point,crs(indic))
veg_point1=veg_point[veg_point$SERIESFIN %in% names(which(table(veg_point$SERIESFIN) < 10)), ]
veg_point1$SERIESFIN=factor(veg_point1$SERIESFIN)
resul = superClass(indic,trainData=veg_point1,tuneLength = 1,,mode='classification',responseCol = "SERIESFIN" , model="rf", na.omit=T)

writeRaster(resul$map,"/Users/stijnhantson/Documents/projects/anza_borrego/veg_mapping/supervised_clasification_RF.tif")
plot(indic[[1]])
points(veg_point)

######## supervised calsification carrizo



planet_mon=list.files("/Users/stijnhantson/Documents/projects/anza_borrego/PLANET/ndvi/",pattern = ".tif$", recursive = TRUE, full.names=T)
img = stack(planet_mon)
ndvi=mean(img)
car_veg=readOGR("/Users/stijnhantson/Documents/projects/anza_borrego/data/carrizo_jef/CarrizoGorgeVeg_PointData-1.shp")
bare=readOGR("/Users/stijnhantson/Documents/projects/anza_borrego/data/carrizo_jef/CarrizoGorgeBareGandR_PointData.shp")

car_veg=car_veg[car_veg$Dom %in% names(which(table(car_veg$Dom) > 5)), ]
  
x = car_veg$Point_X
y = car_veg$Point_Y
x1 = x +3
x2 = x - 3
y1= y+3
y2=y-3

xy1 = cbind(x,y,car_veg$Dom)
xy2 = cbind(x,y1,car_veg$Dom)
xy3 = cbind(x,y2,car_veg$Dom)
xy4 = cbind(x1,y,car_veg$Dom)
xy5 = cbind(x1,y1,car_veg$Dom)
xy6 = cbind(x1,y2,car_veg$Dom)
xy7 = cbind(x2,y,car_veg$Dom)
xy8 = cbind(x2,y1,car_veg$Dom)
xy9 = cbind(x2,y2,car_veg$Dom)

bare$Dom = 20
xy_bare=cbind(bare$POINT_X,bare$POINT_Y,bare$Dom)
xy=as.data.frame(rbind(xy1,xy2,xy3,xy4,xy5,xy6,xy7,xy8,xy9,xy_bare))
dat_2 <- SpatialPointsDataFrame(xy[,c("x", "y")], xy)
crs(dat_2)=crs(car_veg)

#dat_2$V3=factor(dat_2$V3)
car_veg=spTransform(dat_2,crs(img))

car_veg$ndvi = extract(ndvi,car_veg)
bare=car_veg[car_veg$ndvi< -0.1,]
car_veg=car_veg[car_veg$ndvi >= -0.1,]
car_veg=car_veg[car_veg$V3 != 20,]
bare=bare[bare$V3 == 20,]

car_veg=rbind(car_veg,bare)
clas = superClass(img,trainData=car_veg,tuneLength = 1,mode='classification',responseCol = "V3" , model="rf", na.omit=T)

writeRaster(clas$map,"/Users/stijnhantson/Documents/projects/anza_borrego/veg_mapping/supervised_clasification_carrizo_RF.tif")

plot(img[[1]])
points(car_veg)


