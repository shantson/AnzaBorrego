library(raster)
library(gam)


######## clip and reproject precipitation data ######################

list_rec=list.files("/Users/stijnhantson/Documents/projects/anza_borrego/precip/PRISM",pattern = "_??????_bil\\.bil$", recursive = TRUE, full.names=T)
outpath="/Users/stijnhantson/Documents/projects/anza_borrego/precip/clip/"

ext_west<-extent(-120.0, -110, 30, 40)  
ext_anza<-extent(499995, 672225, 3597345, 3734445)  
a <- raster(xmn=500640.6, xmx=672640.6, ymn=3597468, ymx=3733468,res=4000) 
a <- setValues(a, 1:ncell(a))

images=length(list_rec)
i=1
dree1=raster("/Users/stijnhantson/Documents/projects/anza_borrego/DEM_anza/anza_dem_proj.tif")
  sr=crs(dree1)
  
for (i in 477:images){
  len<-nchar(list_rec[i])
  
  year<-substr(list_rec[i], len-13, len-10)
  month<-substr(list_rec[i], len-9, len-8)
  
  outname=paste(outpath,year,"_",month,".tif",sep="")  
  imp=raster(list_rec[i])
  
  imp<-crop(imp,ext_west)
  

  projected_raster <- projectRaster(imp, crs = sr, res=4000)
  imp<-crop(projected_raster,ext_anza)
  imp<- extend(imp,ext_anza)
  imp<-mask(imp,a)
  
  writeRaster(imp,outname)
  imp=0
}
  
  
###### 30m resolution
  
temp=raster("/Users/stijnhantson/Documents/projects/anza_borrego/results/mean_temperature.tif")  
temp2=resample(temp,dem,method="bilinear",filename="/Users/stijnhantson/Documents/projects/anza_borrego/results/mean_temperature_30m.tif")

