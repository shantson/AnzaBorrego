
#script reads in each folder (based on original file .tar.gz file name, see unzip.sh file)
# - deleted original files if there.
# - reads in a set of bands/indexes
# - clips them to the extend of interest and writes the new files to the output directory

library(raster)
#library(insol)
library(stringr)

workdir<-"/Volumes/remote_data/landsat/"
outdir<-"/Volumes/remote_data/anza/"
dir_img<-list.dirs(workdir)  #make list of all folders in directory
nr_img<-length(dir_img)

ext_anza<-extent(499995, 672225, 3597345, 3734445)  
a <- raster(xmn=499995, xmx=672225, ymn=3597345, ymx=3734445,res=30) 
a <- setValues(a, 1:ncell(a))
i<-1
output<-c(0,0,0,0,0,0,0,0,0,0,0,0,0)
output2<-c(0,0,0,0,0,0,0,0,0,0,0,0,0)
output3<-c(0,0,0,0,0,0,0,0,0,0,0,0,0)
output4<-c(0,0,0,0,0,0,0,0,0,0,0,0,0)
output5<-c(0,0,0,0,0,0,0,0,0,0,0,0,0)
output6<-c(0,0,0,0,0,0,0,0,0,0,0,0,0)
output7<-c(0,0,0,0,0,0,0,0,0,0,0,0,0)
output8<-c(0,0,0,0,0,0,0,0,0,0,0,0,0)
output9<-c(0,0,0,0,0,0,0,0,0,0,0,0,0)
output10<-c(0,0,0,0,0,0,0,0,0,0,0,0,0)

dem =raster("/Users/stijnhantson/Documents/projects/anza_borrego/DEM_anza/anza_dem_proj.tif")
dem_slope=terrain(dem, opt='slope', unit='radians', neighbors=8)
dem_aspect=terrain(dem, opt='aspect', unit='radians', neighbors=8)

dre<-read.table("/Users/stijnhantson/Documents/projects/anza_borrego/gridlist_test.txt",header=T)
dre<-SpatialPoints(dre)
crs(dre)<-crs(dem)

for (i in 2:nr_img){   #go through all folders, start with 2 as 1 is workdir
 
img_names<-list.files(dir_img[i])   #make namelist of all files in directory
nr_files<-length(img_names)

len<-nchar(dir_img[i])
len2<-nchar(img_names[1])
lan_v<-substr(img_names[1], len2-47, len2-44)
lan_b<-substr(img_names[2], len2-7, len2)
img_name<-substr(img_names[1], 1, len2-7)
date_img<-substr(img_names[1], len2-30, len2-23)
scene_sensor<-substr(img_names[1], len2-47, len2-32)
sensor <- substr(img_names[1], len2-47, len2-44)

if( any(lan_v == "LC08" | lan_v == "LE07" | lan_v == "LT05" | lan_v == "LT04")){
 if(lan_b == "_b1.tif"){
   print('deleting original files')
  # to check whether original files are included, and if so, delete them
  file.remove(paste(dir_img[i],"/",img_names[2],sep=""))
  file.remove(paste(dir_img[i],"/",img_names[3],sep=""))
  file.remove(paste(dir_img[i],"/",img_names[4],sep=""))
  file.remove(paste(dir_img[i],"/",img_names[5],sep=""))
  file.remove(paste(dir_img[i],"/",img_names[6],sep=""))
  file.remove(paste(dir_img[i],"/",img_names[7],sep=""))
  file.remove(paste(dir_img[i],"/",img_names[8],sep=""))

  if( any(lan_v == "LC08" | lan_v == "LE07") ) {
    file.remove(paste(dir_img[i],"/",img_names[9],sep=""))
    file.remove(paste(dir_img[i],"/",img_names[10],sep=""))
  }
  if (lan_v == "LC08"){
    file.remove(paste(dir_img[i],"/",img_names[11],sep=""))
    file.remove(paste(dir_img[i],"/",img_names[12],sep=""))
  }
    }else{  print('not deleting original files')}
  }else{ print('landsat version not detected')}

##### calculate solar ilumination to detect cast shadows   ##########

f <- readLines(paste(dir_img[i],"/",img_name,"MTL.txt",sep=""))
cline <- grep("SUN_AZIMUTH",f,value=TRUE)
azimuth <- as.numeric(str_extract(cline,"[0-9]+[.0-9]+$"))
cline <- grep("SUN_ELEVATION",f,value=TRUE)
sun_elev <- as.numeric(str_extract(cline,"[0-9]+[.0-9]+$"))
shade=hillShade(test3, test4, angle=sun_elev, direction=azimuth, normalize=FALSE)

##### read mask

imp<-raster(paste(dir_img[i],"/",img_name,"pixel_qa.tif",sep=""))

imp<-crop(imp,ext_anza)
imp<- extend(imp,ext_anza)
imp<-mask(imp,a)
if (lan_v == "LC08"){
imp[imp %in% c(1, 328, 336, 352, 368, 392, 400, 416, 432, 480, 840,848, 864, 880, 904,928, 944, 992, 904, 1348, 1350,1352)] <- NA
}else{
imp[imp %in% c(1, 72, 80, 96, 112, 136, 144, 160, 176, 224)] <- NA
}
cloud_mask<-0
imp[imp>0]<-1
cloud_mask<-imp
cloud_mask[shade < 0.025]<-NA   #### solar ilumination  mask
imp<-0

# read all bands 4 & 5, calculate NDWI, clip & write to output folder
band1<-0
band2<-0
band3<-0
band4<-0
band5<-0
band7<-0

if (lan_v == "LC08"){
  imp<-raster(paste(dir_img[i],"/",img_name,"sr_band2.tif",sep=""))
}else{
  imp<-raster(paste(dir_img[i],"/",img_name,"sr_band1.tif",sep=""))
}
imp<-crop(imp,ext_anza)
imp<- extend(imp,ext_anza)
band1<-mask(imp,a)
if (lan_v == "LC08"){
  band1 <- band1*1.020357266
  print('band1 L8')
}
imp<-0

if (lan_v == "LC08"){
  imp<-raster(paste(dir_img[i],"/",img_name,"sr_band3.tif",sep=""))
}else{
  imp<-raster(paste(dir_img[i],"/",img_name,"sr_band2.tif",sep=""))
}
imp<-crop(imp,ext_anza)
imp<- extend(imp,ext_anza)
band2<-mask(imp,a)
if (lan_v == "LC08"){
  band2 <- band2*1.01363924
}
imp<-0

if (lan_v == "LC08"){
  imp<-raster(paste(dir_img[i],"/",img_name,"sr_band4.tif",sep=""))
  }else{
imp<-raster(paste(dir_img[i],"/",img_name,"sr_band3.tif",sep=""))
}
imp<-crop(imp,ext_anza)
imp<- extend(imp,ext_anza)
band3<-mask(imp,a)
if (lan_v == "LC08"){
  band3 <- band3*1.025717165
}
imp<-0

if (lan_v == "LC08"){
  imp<-raster(paste(dir_img[i],"/",img_name,"sr_band5.tif",sep=""))
}else{
  imp<-raster(paste(dir_img[i],"/",img_name,"sr_band4.tif",sep=""))
}
imp<-crop(imp,ext_anza)
imp<- extend(imp,ext_anza)
band4<-mask(imp,a)
if (lan_v == "LC08"){
  band4 <- band4*0.963490562
}
imp<-0

if (lan_v == "LC08"){
  imp<-raster(paste(dir_img[i],"/",img_name,"sr_band6.tif",sep=""))
}else{
  imp<-raster(paste(dir_img[i],"/",img_name,"sr_band5.tif",sep=""))
}
imp<-crop(imp,ext_anza)
imp<- extend(imp,ext_anza)
band5<-mask(imp,a)
if (lan_v == "LC08"){
  band5 <- band5*0.99755356
}
imp<-0

if (lan_v == "LC08"){
  imp<-raster(paste(dir_img[i],"/",img_name,"sr_band7.tif",sep=""))
}else{
  imp<-raster(paste(dir_img[i],"/",img_name,"sr_band7.tif",sep=""))
}
imp<-crop(imp,ext_anza)
imp<- extend(imp,ext_anza)
band7<-mask(imp,a)
if (lan_v == "LC08"){
  band7 <- band7*1.001416351
}
imp<-0

cloud_mask[band1 > 9999]<-NA
cloud_mask[band2 > 9999]<-NA
cloud_mask[band3 > 9999]<-NA
cloud_mask[band4 > 9999]<-NA
cloud_mask[band5 > 9999]<-NA
cloud_mask[band7 > 9999]<-NA

# read EVI data, clip & write to output folder
evi<-0
evi<-2.5*(((band4/10000)-(band3/10000))/((band4/10000)+6*(band3/10000)-7.5*(band1/10000)+1))
cloud_mask[evi > 9999]<-NA
cloud_mask[evi < -9999]<-NA

#evi<-imp/cloud_mask
imp<-1

# read NBR data, clip & write to output folder
nbr<-0
nbr<-(band4-band7)/(band4+band7)
cloud_mask[nbr > 9999]<-NA
cloud_mask[nbr < -9999]<-NA

#evi<-imp/cloud_mask
imp<-1

# calculate and write NDWI out
ndwi<-0
ndwi<-((band4-band5)/(band4+band5))
cloud_mask[ndwi > 0.9999]<-NA
cloud_mask[ndwi < -0.9999]<-NA

ndvi<-0
ndvi<-((band4-band3)/(band4+band3))
cloud_mask[ndvi > 0.9999]<-NA
cloud_mask[ndvi < -0.9999]<-NA


ndvi<-(ndvi*10000)/cloud_mask
ndwi<-(ndwi*10000)/cloud_mask
evi<-(evi*10000)/cloud_mask
nbr<-(nbr*10000)/cloud_mask
band1<-band1/cloud_mask
band2<-band2/cloud_mask
band3<-band3/cloud_mask
band4<-band4/cloud_mask
band5<-band5/cloud_mask
band7<-band7/cloud_mask


outname<-paste(outdir,date_img,"_",scene_sensor,"_ndwi.tif", sep="")
writeRaster(ndwi,outname)
outname<-paste(outdir,date_img,"_",scene_sensor,"_ndvi.tif", sep="")
writeRaster(ndvi,outname)
outname<-paste(outdir,date_img,"_",scene_sensor,"_nbr.tif", sep="")
writeRaster(nbr,outname)
outname<-paste(outdir,date_img,"_",scene_sensor,"_evi.tif", sep="")
writeRaster(evi,outname)
outname<-paste(outdir,date_img,"_",scene_sensor,"_mask.tif", sep="")
writeRaster(cloud_mask,outname,overwrite=T)

outname<-paste(outdir,date_img,"_",scene_sensor,"_b1.tif", sep="")
writeRaster(band1,outname)
outname<-paste(outdir,date_img,"_",scene_sensor,"_b2.tif", sep="")
writeRaster(band2,outname)
outname<-paste(outdir,date_img,"_",scene_sensor,"_b3.tif", sep="")
writeRaster(band3,outname)
outname<-paste(outdir,date_img,"_",scene_sensor,"_b4.tif", sep="")
writeRaster(band4,outname)
outname<-paste(outdir,date_img,"_",scene_sensor,"_b5.tif", sep="")
writeRaster(band5,outname)
outname<-paste(outdir,date_img,"_",scene_sensor,"_b7.tif", sep="")
writeRaster(band7,outname)

outp<-0
outp<-extract(evi,dre)
outp<-c(outp,sensor,date_img)
output<-rbind(output,outp)

outp<-0
outp<-extract(ndwi,dre)
outp<-c(outp,sensor,date_img)
output2<-rbind(output2,outp)     

outp<-0
outp<-extract(nbr,dre)
outp<-c(outp,sensor,date_img)
output3<-rbind(output3,outp) 

outp<-0
outp<-extract(ndvi,dre)
outp<-c(outp,sensor,date_img)
output4<-rbind(output4,outp) 

outp<-0
outp<-extract(band1,dre)
outp<-c(outp,sensor,date_img)
output5<-rbind(output5,outp) 

outp<-0
outp<-extract(band2,dre)
outp<-c(outp,sensor,date_img)
output6<-rbind(output6,outp) 

outp<-0
outp<-extract(band3,dre)
outp<-c(outp,sensor,date_img)
output7<-rbind(output7,outp) 

outp<-0
outp<-extract(band4,dre)
outp<-c(outp,sensor,date_img)
output8<-rbind(output8,outp) 

outp<-0
outp<-extract(band5,dre)
outp<-c(outp,sensor,date_img)
output9<-rbind(output9,outp) 

outp<-0
outp<-extract(band7,dre)
outp<-c(outp,sensor,date_img)
output10<-rbind(output10,outp) 


 }

write.table(output,"/Users/stijnhantson/Documents/projects/anza_borrego/evi2.txt")
write.table(output2,"/Users/stijnhantson/Documents/projects/anza_borrego/ndwi2.txt")
write.table(output3,"/Users/stijnhantson/Documents/projects/anza_borrego/nbr2.txt")
write.table(output4,"/Users/stijnhantson/Documents/projects/anza_borrego/ndvi2.txt")

write.table(output5,"/Users/stijnhantson/Documents/projects/anza_borrego/band1.txt")
write.table(output6,"/Users/stijnhantson/Documents/projects/anza_borrego/band2.txt")
write.table(output7,"/Users/stijnhantson/Documents/projects/anza_borrego/band3.txt")
write.table(output8,"/Users/stijnhantson/Documents/projects/anza_borrego/band4.txt")
write.table(output9,"/Users/stijnhantson/Documents/projects/anza_borrego/band5.txt")
write.table(output10,"/Users/stijnhantson/Documents/projects/anza_borrego/band7.txt")
