
library(raster)

######### produce monthly median NDVI ##############


t=raster("F:/orange/19821117_LT04_L1TP_040037_b2.tif")
t[] <- NA

outdir="F:/orange_monthly/"
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
    
    list_rec=list.files("F:/orange",pattern = dr, recursive = TRUE, full.names=T)
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
write.table(dat,"F:/orange_nr_img_month.txt",sep=" ") 

