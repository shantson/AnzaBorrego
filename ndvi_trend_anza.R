library(raster)
library(LSD)

#######  produce annual summer NDVI images   ##################
outdir="/Users/stijnhantson/Documents/projects/anza_borrego/annual_june_sept/"
year=1984
for (year in 1984:2017){
dr=paste("^",year,"[0][0,6-9].._[[:alnum:]]+_[[:alnum:]]+_[[:alnum:]]+_ndvi",sep="")

#list_rec=list.files("/Users/stijnhantson/Documents/projects/anza_borrego/test",pattern = "^1985[0-1][0,7-9].._[[:alnum:]]+_[[:alnum:]]+_[[:alnum:]]+_b1", recursive = TRUE, full.names=T)
list_rec=list.files("/Volumes/remote_data/anza",pattern = dr, recursive = TRUE, full.names=T)

all_img= stack(list_rec)
#median_img= mean(all_img, na.rm=TRUE)
#plot(median_img)
r_median <- calc(all_img, median, na.rm=TRUE)
r_stdev <- calc(all_img,sd, na.rm=TRUE)

outname=paste(outdir,year,"summer_ndvi.tif",sep="")
writeRaster(r_median,outname)
outname1=paste(outdir,year,"summer_ndvi_stdev.tif",sep="")
writeRaster(r_stdev,outname1)
}

######### produce monthly median NDVI ##############


t=raster("/Users/stijnhantson/Documents/projects/anza_borrego/annual/2017summer_ndvi.tif")
t[] <- NA

outdir="/Users/stijnhantson/Documents/projects/anza_borrego/monthly/"
year=1984
month=0
month1=0
dm=0
yr=0
mon=0
nr_img=0

for (year in 1984:2017){
  for (month in 1:12){
    if (month < 10){
      month1=paste("0",month,sep="")
    } else {
      month1=month
    }
  dr=paste("^",year,month1,".._[[:alnum:]]+_[[:alnum:]]+_[[:alnum:]]+_ndvi",sep="")

  list_rec=list.files("/Volumes/remote_data/anza",pattern = dr, recursive = TRUE, full.names=T)
  r_median=0
  if (length(list_rec) == 0){
    r_median=t
  }else if (length(list_rec) == 1) {
    r_median=raster(list_rec[1])
  } else {
  
  all_img= stack(list_rec)
  #median_img= mean(all_img, na.rm=TRUE)
  #plot(median_img)
  r_median <- calc(all_img, median, na.rm=TRUE)

    }
dm=dm+1
   nr_img[dm] =length(list_rec)
   mon[dm]=month
   yr[dm]=year

 
   outname=paste(outdir,year,month1,"median_ndvi.tif",sep="")
  writeRaster(r_median,outname)
  
    } 
}
  dat= cbind(yr, mon, nr_img)
write.table(dat,"/Users/stijnhantson/Documents/projects/anza_borrego/nr_img_month.txt",sep=" ")  



##############  calculated trend in summer ndvi    #######################

list_rec=list.files("/Users/stijnhantson/Documents/projects/anza_borrego/annual/", recursive = TRUE, full.names=T)
lm_fun = function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ tim); summary(m)$coefficients[2] }} # slope

lm_fun = function(x) { m = lm(x ~ tim,na.rm=T); summary(m)$coefficients[2] } # slope

precip_s1=raster::stack(list_rec)
precip_s3=raster::brick(precip_s1)
tim=(1984:2017)
tim<-as.Date(tim, format = "%Y")
annual_ave_stack.slope= calc(precip_s1, lm_fun)
writeRaster(annual_ave_stack.slope,"/Users/stijnhantson/Documents/projects/anza_borrego/summer_trend_ndvi_v2.tif")

mean_ndvi=mean(precip_s1)
mean_ndvi_zero=abs(mean_ndvi)
relative_ndvi_change=(annual_ave_stack.slope/mean_ndvi_zero)*100

writeRaster(relative_ndvi_change,"/Users/stijnhantson/Documents/projects/anza_borrego/relative_summer_change_ndvi.tif")


dem=raster("/Users/stijnhantson/Documents/projects/anza_borrego/DEM_anza/anza_dem_proj.tif")
reg1=stack(dem,relative_ndvi_change)
v<- data.frame(values(reg1))
names(v)<-c("L1","L2")
sub<-v[sample(nrow(v), 100000),]
heatscatter(sub$L1,sub$L2,ylim=c(-3,3),xlim=c(-60,2000))


#################  monthly NDVI trend ##############

list_rec=list.files("/Users/stijnhantson/Documents/projects/anza_borrego/monthly/", recursive = TRUE, full.names=T)

lm_fun = function(x) { m = lm(x ~ tim,na.rm=T); summary(m)$coefficients[2] } # slope

precip_s1=raster::stack(list_rec)
precip_s3=raster::brick(precip_s1)
tim=(1:408)

annual_ave_stack.slope= calc(precip_s1, lm_fun)
annual_ave_stack.slope=annual_ave_stack.slope*12
writeRaster(annual_ave_stack.slope,"/Users/stijnhantson/Documents/projects/anza_borrego/monthly_trend_ndvi.tif")



mean_ndvi=mean(precip_s1, na.rm=TRUE)
mean_ndvi_zero=abs(mean_ndvi)
relative_ndvi_change=(annual_ave_stack.slope/mean_ndvi_zero)*100

writeRaster(relative_ndvi_change,"/Users/stijnhantson/Documents/projects/anza_borrego/relative_monthly_change_ndvi.tif", overwrite=T)



#####################################################

funa <- function(y) { 
  if(all(is.na(y))) {
    c(NA, NA)
  } else {
    lm(y ~ x)$coefficients 
  }
}
x <- c(1:length(tim))
r <- calc(precip_s1, funa)


fun <- function(x) { lm(x ~ tim)$coefficients[2] }
x2 <- calc(precip_s3, fun)


plot(r,1)
plot(r,2)


#############  extract individual pixels summer NDVI time series  #################

point=SpatialPoints(data.frame(cbind(568159,3635823)))
crs(point)=crs(precip_s1)
precip_p=extract(precip_s1,point)
test1=data.frame(precip_p)
test2=t(test1)
precip_s2=ts(test2, start = 1984, frequency = 1)
plot(precip_s2)


