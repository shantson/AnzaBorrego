
library(raster)

library(doParallel)  #Foreach Parallel Adaptor 
library(foreach)     #Provides foreach looping construct
UseCores <- detectCores() -1

month=1
for (month in 1:12){
if (month < 10){
month1=paste("0",month,sep="")
}else{
  month1=month
}
  dirs=paste("/Volumes/MyBookDuo/PLANET/","dat",month1,"/",sep="")
  dir.create(dirs)
dr=paste(month1,"+_[[:alnum:]]+_[[:alnum:]]+-",month1,"+-[[:alnum:]]+_[[:alnum:]]+_BGRN_Analytic.tif",sep="")

imgs = list.files("/Volumes/MyBookDuo/PLANET/data/",recursive = TRUE, full.names=T, pattern = "+_[[:alnum:]]+_[[:alnum:]]+-01+-[[:alnum:]]+_[[:alnum:]]+_BGRN_Analytic.tif")

len = length(imgs)
for (i in 1:len){
bands=1
for (bands in 1:4){
  
  rr =  raster(imgs[i], band = bands)
  rr[rr == 0] <- NA
  name = paste(dirs,i,"_band",bands,".tif",sep="")
  writeRaster(rr,name, overwrite =T)
}
}
}



month=1
for (month in 1:12){
  if (month < 10){
    month1=paste("0",month,sep="")
  }else{
    month1=month
  }
  dirs=paste("/Volumes/MyBookDuo/PLANET/","dat",month1,"/",sep="")
  bands=1
  
  cl<- makeCluster(UseCores)
  registerDoParallel(cl)
 
  foreach (bands =1:4) %dopar% {
    library(raster)
    ds = paste("band",bands,".tif",sep="")
    dr = list.files(dirs,recursive = TRUE, full.names=T, pattern = ds)
    
    rast.mosaic = 0

    r.list=1
    r.list <- list()
    for(rr in 1:length(dr)){  
      r.list[[rr]] <- raster(dr[rr])  
    } 
    
    r.list$fun <- median
    rast.mosaic <- do.call(mosaic,r.list)
    
    nam = paste("/Volumes/MyBookDuo/PLANET/","month",month1,"_",bands,".tif",sep="")
    writeRaster(rast.mosaic,nam, overwrite=T)
  }
  stopCluster(cl)
  }




i=1
for (i in 1:len){
  len2<-nchar(imgs[i])
  month<-substr(imgs[i], len2-27, len2-26)
  month = as.numeric(month)
  if (month > 4){
    summer = rbind(summer,imgs[i])
  }else{
    spring = rbind(spring,imgs[i])
  }
}

summer=summer[-1]
spring=spring[-1]


len1 = length(summer)

for (p in 1:len1){
 # k=p+20
  for (bands in 1:4){
    
  rr =  raster(summer[p], band = bands)
  rr[rr == 0] <- NA
  name = paste("/Users/stijnhantson/Documents/projects/anza_borrego/PLANET/summer/","summer_",p,"_band",bands,".tif",sep="")
  writeRaster(rr,name, overwrite =T)
  }
}

len2 = length(spring)

for (p in 1:len2){
  k=p
  for (bands in 1:4){
    
    rr =  raster(spring[p], band = bands)
    rr[rr == 0] <- NA
    name = paste("/Users/stijnhantson/Documents/projects/anza_borrego/PLANET/spring/","spring_",k,"_band",bands,".tif",sep="")
    writeRaster(rr,name, overwrite =T)
  }
}


bands=1
for (bands in 1:4){
  rast.mosaic = 0
   na = paste("_band",bands,".tif$",sep="")
  dr = list.files("/Users/stijnhantson/Documents/projects/anza_borrego/PLANET/summer/", pattern = na, full.names=T)
r.list=1
  r.list <- list()
  for(i in 1:length(dr)){  
    r.list[[i]] <- raster(dr[i])  
  } 
  
  r.list$fun <- median
  rast.mosaic <- do.call(mosaic,r.list)
  
  nam = paste("/Users/stijnhantson/Documents/projects/anza_borrego/PLANET/summer_mosaic_band",bands,".tif",sep="")
  writeRaster(rast.mosaic,nam, overwrite=T)
}

for (bands in 1:4){
  rast.mosaic = 0
  na = paste("_band",bands,".tif$",sep="")
  dr = list.files("/Users/stijnhantson/Documents/projects/anza_borrego/PLANET/spring/", pattern = na, full.names=T)
  r.list=1
   r.list <- list()
  for(i in 1:length(dr)){  
    r.list[[i]] <- raster(dr[i])  
  } 
  
  r.list$fun <- median
  rast.mosaic <- do.call(mosaic,r.list)
  
  nam = paste("/Users/stijnhantson/Documents/projects/anza_borrego/PLANET/spring_mosaic_band",bands,".tif",sep="")
  writeRaster(rast.mosaic,nam, overwrite=T)
}

##### calculate monthly ndvi from planet #######

for (month in 1:12){
  if (month < 10){
    month1=paste("0",month,sep="")
  }else{
    month1=month
  }
  b3=paste("/Volumes/MyBookDuo/PLANET/month",month1,"_3.tif",sep="")
  b4=paste("/Volumes/MyBookDuo/PLANET/month",month1,"_4.tif",sep="")  
  band3=raster(b3)
  band4=raster(b4)
  ndvi=(band4-band3)/(band4+band3)
  nam = paste("/Volumes/MyBookDuo/PLANET/ndvi/",month1,"ndvi.tif",sep="")
  writeRaster(ndvi,nam)
}


###### classify carizzo gorge  #############


dr = list.files("/Volumes/MyBookDuo/PLANET/ndvi/", full.names=T)
carizo_ndvi = brick(dr)




