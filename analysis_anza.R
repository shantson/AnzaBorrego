library(raster)
library(gam)
library(car)

list_rec2=list.files("/Users/stijnhantson/Documents/projects/anza_borrego/precip/clip",pattern = ".tif$", full.names=T)
start=6
for (year in 1981:2017){
end = start + 2
dr = stack(list_rec2[start:end])
pri = sum(dr)
print(start)
  start = start+12
  
out= paste("/Users/stijnhantson/Documents/projects/anza_borrego/precip/June-aug/",year,".tif",sep="")
writeRaster(pri,out,overwrite=T)
}

precip = raster("/Users/stijnhantson/Documents/projects/anza_borrego/results/winter_precip_summer_ndvi_precip1lag3temp_rsquare2.tif")
preciplag = raster("/Users/stijnhantson/Documents/projects/anza_borrego/results/winter_precip_summer_ndvi_precip_temp_rsquare2.tif")
temp = raster("/Users/stijnhantson/Documents/projects/anza_borrego/results/winter_precip_summer_ndvi_precip_precip1lag3_rsquare2.tif")
all = raster("/Users/stijnhantson/Documents/projects/anza_borrego/results/winter_precip_summer_ndvi_precip_precip1lag3temp_rsquare2.tif")

r_temp = ((all-temp)/all)*100
r_prec = ((all-precip)/all)*100
r_preclag = ((all-preciplag)/all)*100

plot(r_prec, zlim=c(0,100))
plot(r_preclag, zlim=c(0,100))
plot(r_temp, zlim=c(0,100))


list_rec1=list.files("/Users/stijnhantson/Documents/projects/anza_borrego/annual/",pattern = ".tif$", recursive = TRUE, full.names=T)
list_rec2=list.files("/Users/stijnhantson/Documents/projects/anza_borrego/precip/annual_oct",pattern = ".tif$", recursive = TRUE, full.names=T)
list_rec3=list.files("/Users/stijnhantson/Documents/projects/anza_borrego/temp/annual_oct",pattern = ".tif$", recursive = TRUE, full.names=T)


list_rec1=list.files("/Users/stijnhantson/Documents/projects/anza_borrego/annual/",pattern = ".tif$", recursive = TRUE, full.names=T)
list_rec2=list.files("/Users/stijnhantson/Documents/projects/anza_borrego/precip/oct_mar",pattern = ".tif$", recursive = TRUE, full.names=T)
list_rec3=list.files("/Users/stijnhantson/Documents/projects/anza_borrego/temp/june_sep",pattern = ".tif$", recursive = TRUE, full.names=T)



sr1=stack(list_rec1)
sr2=stack(list_rec2[2:36])
sr3=stack(list_rec3[2:36])

#dem=raster("/Users/stijnhantson/Documents/projects/anza_borrego/precip/perc_precip_change_30m.tif")
#sr2=resample(sr2,dem,method="bilinear")

mean_pr=mean(sr2)
mean_ndvi

y = c(3630198)
x = c(570022)


y = c(3653854)
x = c(592898)

y = c(3660854)
x = c(555898)

y = c(3621577)
x = c(572101)

y = c(3618597)
x = c(582334)

y = c(3626300)
x = c(594052)

y = c(3651698)
x = c(616248)

y = c(3655035)
x = c(548175)

y = c(3669811)
x = c(548822)

y = c(3699061)
x = c(544948)

#test
y = c(3646526)
x = c(579361)

dr = cbind(x,y)
mdf=as.data.frame(dr)
xy <- mdf[,c(1,2)]

spdf <- SpatialPointsDataFrame(coords = xy, data= mdf,
                               proj4string = CRS("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

ts1 = (extract(sr1,spdf))/10000
ts2 = extract(sr2,spdf)
ts3 = extract(sr3,spdf)

ts1 = t(ts1)
ts2 = t(ts2)
ts3 = t(ts3)

tr=((ts2[3:35])^2) 
rss = lm(ts1[2:34]~ts2[3:35] + ts2[2:34] + ts3[3:35])
summary(rss)
mean_precip=mean(ts2)
#plot(ts2[3:35],ts1[2:34])

dat = seq(as.Date("1985/01/01"), as.Date("2017/01/01"), "years")

plot(dat,ts1[2:34],type="l", ylab="summer NDVI",cex.axis=1.5)
abline(lm(ts1[2:34]~dat))
plot(dat,ts2[3:35],type="l", ylab="precipitation")
abline(lm(ts2[3:35]~dat))
plot(dat,ts3[3:35],type="l", ylab="temperature")

plot(dat,resid(rss))

plot(ts2[3:16],ts1[2:15], xlim = c(0,700), ylim=c(0.05,0.25), col="black", xlab = "precipitation (mm)", ylab="summer NDVI", pch = 16,cex = 1.5,cex.lab=1.25, cex.axis = 1.25)
points(ts2[17:35],ts1[16:34], xlim = c(0,700), ylim=c(0.05,0.25),cex=1.5, pch = 16)
legend("bottomright",c("1985-1998","1999-2017"),col=c("blue","black"), pch = 16,cex = 1.3)

plot(ts2[3:16],ts1[2:15], xlim = c(0,450), ylim=c(0.07,0.14), col="blue", xlab = "precipitation (mm)", ylab="summer NDVI", pch = 16,cex = 1.5,cex.lab=1.25, cex.axis = 1.25)
points(ts2[17:35],ts1[16:34], xlim = c(0,450), ylim=c(0.07,0.14),cex=1.5, pch = 16)
legend("bottomright",c("1985-1998","1999-2017"),col=c("blue","black"), pch = 16,cex = 1.3)

plot(ts2[3:16],ts1[2:15], xlim = c(0,700), ylim=c(0,0.300), col="blue", xlab = "precipitation (mm)", ylab="summer NDVI", pch = 16)
points(ts2[17:35],ts1[16:34], xlim = c(0,700), ylim=c(0,300), pch = 16)

test2=as.data.frame(cbind(ts1[2:34],ts2[3:35],ts2[2:34],ts3[2:34]))
model <- nls( V1 ~ (STA*((V2)^TRE)+ PD*V4), start=list(STA=2, TRE=1,PD= 0), data=test2, control = list(maxiter = 500))

rss = lm(ts1[2:34]~ts2[3:35]+ ts3[3:35])

rss = lm(ts1[1:15]~ts2[2:16])
summary(rss)
rss = lm(ts1[16:34]~ts2[17:35])
summary(rss)

lmtest::bptest(rss)
car::ncvTest(rss)

res1 =   unlist(resid(rss))

dr=c(1:33)
test1 =(cbind(res1,dr))
rrr =lm(test1[,1] ~test1[,2])
pre = summary(rrr)

t.test(res1[1:14],res1[15:33])$p.value


is.infinite(test1[,1])






sl1=raster("/Users/stijnhantson/Documents/projects/anza_borrego/results/summer_ndvi1985-1998_precip_slope.tif")
sl2=raster("/Users/stijnhantson/Documents/projects/anza_borrego/results/summer_ndvi1999-2017_precip_slope2.tif")
dif=sl2-sl1
writeRaster(dif,"/Users/stijnhantson/Documents/projects/anza_borrego/results/dif_slope_dry-wet_period.tif")

resi_slope = raster("/Users/stijnhantson/Documents/projects/anza_borrego/results/summer_ndvi_precip_precip1lagtemp_slope_residu_slope.tif")
p_ttest = raster("/Users/stijnhantson/Documents/projects/anza_borrego/results/summer_ndvi_precip_precip1lagtemp_slope_ttest_p.tif")
p_slope = raster("/Users/stijnhantson/Documents/projects/anza_borrego/results/summer_ndvi_precip_precip1lagtemp_slope_residu_p.tif")

rsq = raster("/Users/stijnhantson/Documents/projects/anza_borrego/results/summer_ndvi_precip_precip1lagtemp_rsquare2.tif")

plot(dif, zlim = c(-1.5,1.5))
plot(p_ttest, zlim = c(0,1))
plot(p_slope, zlim = c(0,1))
resi_slope[p_slope > 0.1] <- NA
resi_slope1 = resi_slope
resi_slope1[resi_slope>0] = 1

resi_slope1[resi_slope<0] = -1
plot(rsq)
plot(resi_slope)

######## clip and reproject precipitation data ######################

list_rec=list.files("/Users/stijnhantson/Documents/projects/anza_borrego/precip/",pattern = "_??????_bil\\.bil$", recursive = TRUE, full.names=T)
outpath="/Users/stijnhantson/Documents/projects/anza_borrego/precip/clip/"

ext_west<-extent(-120.0, -110, 30, 40)  
ext_anza<-extent(499995, 672225, 3597345, 3734445)  
a <- raster(xmn=500640.6, xmx=672640.6, ymn=3597468, ymx=3733468,res=4000) 
a <- setValues(a, 1:ncell(a))

images=length(list_rec)
i=1

for (i in 1:images){
  len<-nchar(list_rec[i])
  
  year<-substr(list_rec[i], len-13, len-10)
  month<-substr(list_rec[i], len-9, len-8)
  
  outname=paste(outpath,year,"_",month,".tif",sep="")  
  imp=raster(list_rec[i])
  
  imp<-crop(imp,ext_west)
  
  sr=crs(dree1)
  projected_raster <- projectRaster(imp, crs = sr, res=4000)
  imp<-crop(projected_raster,ext_anza)
  imp<- extend(imp,ext_anza)
  imp<-mask(imp,a)
  
  writeRaster(imp,outname)
  imp=0
}


############### extract PRISM precipitation data for meteo stations ###################

list_rec=list.files("/Users/stijnhantson/Documents/projects/anza_borrego/precip/clip/",pattern = "^[1-2].*", recursive = TRUE, full.names=T)

precip_s1=raster::stack(list_rec[1:432])
precip_s3=raster::brick(precip_s1)

meteo_obs=read.table("/Users/stijnhantson/Documents/projects/anza_borrego/precip/precip_site.txt",header = T)

#sp <- SpatialPoints(data.frame(cbind(550000,3650000)))
sp <- SpatialPoints(data.frame(cbind(meteo_obs$Lon,meteo_obs$Lat)))
crs(sp)=crs(imp)
sr=crs(precip_s3)
sp <- spTransform(sp, sr)
crs(sp)

precip_p=extract(precip_s3,sp)
test1=data.frame(precip_p)
test2=t(test1)
precip_s2=ts(test2, start = c(1981,7), frequency = 12)

plot(precip_s2)
write.table(test1,"/Users/stijnhantson/Documents/projects/anza_borrego/precip/test.txt",sep=" ")


########## evaluate PRISM precipitation   ###################
expl=read.table("/Users/stijnhantson/Documents/projects/anza_borrego/precip/prism_obs.txt", sep="\t", header=T)

tiff(file="/Users/stijnhantson/Documents/projects/anza_borrego/precip/prism_validation.tif",width=2500,height=2500, res=350)

par(mfrow=c(3,3))
plot(expl$obs1,expl$X1, xlim = c(0,150), ylim=c(0,150))
plot(expl$obs2,expl$X2, xlim = c(0,100), ylim=c(0,100))
plot(expl$obs3,expl$X3, xlim = c(0,100), ylim=c(0,100))
plot(expl$obs4,expl$X4, xlim = c(0,700), ylim=c(0,700))
plot(expl$obs5,expl$X5, xlim = c(0,250), ylim=c(0,250))
plot(expl$obs6,expl$X6, xlim = c(0,140), ylim=c(0,140))
plot(expl$obs7,expl$X7, xlim = c(0,350), ylim=c(0,350))
dev.off()

######### trends in precipitation   #############
list_rec=list.files("/Users/stijnhantson/Documents/projects/anza_borrego/precip/clip/",pattern = "^[1-2].*", recursive = TRUE, full.names=T)
r=length(list_rec)
precip_s1=raster::stack(list_rec[1:432])   #only until 2016

mean_precip=(mean(precip_s1)*12)
plot(mean_precip)

funa <- function(y) { 
  if(all(is.na(y))) {
    c(NA, NA)
  } else {
    lm(y ~ x)$coefficients 
  }
}
x <- c(1:432)
r <- calc(precip_s1, funa)

plot(r,1)
plot(r,2)

da=resample(mean_precip,test, method="bilinear",filename="/Users/stijnhantson/Documents/projects/anza_borrego/results/mean_precip_30m.tif")

########### relate NDVI to precipitation   ###############

# precip_s3   is PRISM raster stack

list_rec=list.files("/Users/stijnhantson/Documents/projects/anza_borrego/precip/clip/",pattern = "^[1-2].*", recursive = TRUE, full.names=T)
r=length(list_rec)
precip_s1=raster::stack(list_rec[10:r])   #start in July
precip_s3=raster::brick(precip_s1)

dre<-read.table("/Users/stijnhantson/Documents/projects/anza_borrego/gridlist_test.txt",header=T)
dre<-SpatialPoints(dre)

precip_p=extract(precip_s3,dre)
test1=data.frame(precip_p)
test2=t(test1)

prec2=zooreg(test2[,5],start = c(1982,1), frequency = 12) ## take care, starts in july
s2<- zoo(subset(prec2, (cycle(prec2) %in% 1:12)))

as.year <- function(x) as.numeric(floor(as.yearmon(x)))
sum_prec <- as.ts(aggregate(as.zoo(s2), as.year, FUN=sum))
sum_prec2=window(sum_prec,start = c(1984), end = c(2017))

tes=data.frame(spring)
##### precipitation with one year lag

prec3=zooreg(test2[,5],start = c(1983,1), frequency = 12)
as.year <- function(x) as.numeric(floor(as.yearmon(x)))
sum_prec <- as.ts(aggregate(as.zoo(prec3), as.year, FUN=sum))
precip_min1=window(sum_prec,start = c(1985), end = c(2017))


######## data analysis

res=glm(summer~sum_prec2+test3+summermin1)
summary(res)

test3=(sum_prec2)^2
plot(predict(res),summer,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)

res=glm(spring~sum_prec2 +springmin1)+test3)
summary(res)

test3=(sum_prec2)^2
plot(predict(res),spring,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)



plot(sum_prec2,summer)
plot(log10(sum_prec2),summer)
plot(sum_prec2,summerdif)
G = gam(spring~s(sum_prec2)+s(test3)+s(precip_min1))
b <- gam(G~G)
print(b)
plot(G,se = TRUE)
plot(G,pages=1, se = TRUE, all.terms=TRUE)


plot(sum_prec2,precip_min1)

#zm2=na.spline(zm) time series comes now from previous routine
plot(zm2)
ts2=zooreg(zm2$X.1, start = c(1984,4), frequency = 12)

plot(zm2)
zm3=window(as.ts(ts2),start = c(1985,1), end = c(2017,12))
zm4=zooreg(zm3, start = c(1985,1), frequency = 12)
s<- zoo(subset(zm4, (cycle(zm4) %in% 2:5)))
s2<- zoo(subset(zm4, (cycle(zm4) %in% 7:10)))

as.year <- function(x) as.numeric(floor(as.yearmon(x)))
spring <- as.ts(aggregate(as.zoo(s), as.year, FUN="quantile",p=.90))
summer <- as.ts(aggregate(as.zoo(s2), as.year, median))

plot(summer)

###### now the same with a one year time lag


zm3=window(as.ts(ts2),start = c(1984,4), end = c(2016,12))
zm4=zooreg(zm3, start = c(1985,4), frequency = 12)
s2<- zoo(subset(zm4, (cycle(zm4) %in% 7:10)))

as.year <- function(x) as.numeric(floor(as.yearmon(x)))
summermin1 <- as.ts(aggregate(as.zoo(s2), as.year, median))
summerdif=summer-summermin1

plot(summermin1)



### calculate temporal trend for precip data



##### time lag analysis

laggedTS <- lag(tsData, 3) # shifted 3 periods earlier. Use “-3″ to shift by 3 periods forward (later).



