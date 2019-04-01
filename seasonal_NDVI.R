library(raster)

t=raster("F://march_minoctober_trend_ndvi.tif")
t[] <- NA

outdir="F://seasonal/"
year=1984
month=0
month1=0
dm=0
yr=0
mon=0
nr_img=0
d=0
list_rec1=0
season=0 
dm=0

for (year in 1993:2017){
  
  for (i in 1:4){
  last_month=i*3
  list_rec1= ""
  for (d in 1:3){
   month=(last_month+1)-d
     if (month < 10){
      month1=paste("0",month,sep="")
    } else {
      month1=month
    }
    dr=paste("^",year,month1,".._[[:alnum:]]+_[[:alnum:]]+_[[:alnum:]]+_ndvi.tif$",sep="")
    
    list_rec=list.files("F://anza/",pattern = dr, recursive = TRUE, full.names=T)
    list_rec1=c(list_rec1,list_rec)
  }  
  list_rec1 = list_rec1[-1]
    if (length(list_rec1) == 0){
      r_median=t
    }else if (length(list_rec1) == 1) {
      r_median=raster(list_rec1[1])
    } else {
      
      all_img= stack(list_rec1)
      #median_img= mean(all_img, na.rm=TRUE)
      #plot(median_img)
      r_median <- calc(all_img, median, na.rm=TRUE)
      
    }
    dm=dm+1
    nr_img[dm] =length(list_rec1)
    mon[dm]=i
    yr[dm]=year
    
    
    if (i == 1){
      season="jan_march"
    }else{
      if(i == 2){
        season="april_june"
      }else{
        if(i == 3){
          season="july_sept"
        }else{
          if(i == 4){
            season="oct_dec"
          }}}}
    
    
    outname=paste(outdir,year,"_",season,"_median_ndvi.tif",sep="")
    writeRaster(r_median,outname)
    
  }
}
dat= cbind(yr, mon, nr_img)
write.table(dat,"F://nr_img_season2.txt",sep=" ") 






################ precip seasonal
lm_fun = function(x) {if (all(is.na(x))){NA} else {m = lm(x ~ tim); summary(m)$coefficients[2] }} # slope

i=1
season=0
for (i in 1:4){
  
  if (i == 1){
    season="jan_march"
  }else{
    if(i == 2){
      season="april_june"
    }else{
      if(i == 3){
        season="july_sept"
      }else{
        if(i == 4){
          season="oct_dec"
        }}}}
  
  
  
  list_rec=list.files("F:/seasonal/",pattern = season, recursive = TRUE, full.names=T)
  
  precip_s1=raster::stack(list_rec[10:34])
  nr_timestep=length(list_rec[10:34])
  tim=(1:nr_timestep)
  annual_ave_stack.slope= calc(precip_s1, lm_fun)
  mean_ndvi=mean(abs(precip_s1), na.rm=TRUE)
  relative_change=(annual_ave_stack.slope/mean_ndvi)*100
  
  outname=paste("F:/realtive_NDVI_trend_1993_",season,".tif",sep="")
  writeRaster(relative_change,outname,overwrite=TRUE)

  
}



