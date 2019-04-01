
library("raster")
library("cluster")
library("randomForest")

list_rec=list.files("F:/monthly/",pattern = ".tif$", recursive = TRUE, full.names=T)

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


indic=stack(Q05,Q95,Q50,sd_annual,Q95minQ05,meanminQ50)
v <- getValues(indic)
vx<-v[sample(nrow(v), 10000),]
rf = randomForest(vx)
rf_prox <- randomForest(vx,ntree = 1000, proximity = TRUE)$proximity

E_rf <- kmeans(rf_prox, 100, iter.max = 200, nstart = 10)
rf <- randomForest(vx,as.factor(E_rf$cluster),ntree = 500)
rf_raster<- predict(indic,rf)
plot(rf_raster)



writeRaster(rf_raster,"F:/random_forest_class2.tif",overwrite=TRUE)


