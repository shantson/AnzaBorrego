


library(raster)
library(LSD)

#######  produce annual spring NDVI images   ##################
# calculate the difference between spring and previous summer ndvi, and sum over spring period

indir_summer="/Users/stijnhantson/Documents/projects/anza_borrego/annual/"
indir_month="/Users/stijnhantson/Documents/projects/anza_borrego/monthly/"
i=3
year=1993
for (year in 1985:2017){
year_pre=year-1
dra=paste(indir_summer,year_pre,"summer_ndvi.tif",sep="")
img_name1=raster(dra)
dra=0
dra=paste(indir_month,year,"10","median_ndvi.tif",sep="")
oct_img=raster(dra)

dre=paste(indir_month,year,"0",i,"median_ndvi.tif",sep="")
img_name2=raster(dre)
dif_img=img_name2 - img_name1
#dif_img[dif_img < 0] <- 0
#sum_img=sum_img+dif_img

nameout=paste("/Users/stijnhantson/Documents/projects/anza_borrego/spring/",year,"_march_minus_summer_median_ndvi.tif",sep="")
writeRaster(dif_img,nameout)


dif_img=img_name2 - oct_img
#dif_img[dif_img < 0] <- 0
#sum_img=sum_img+dif_img

nameout=paste("/Users/stijnhantson/Documents/projects/anza_borrego/spring/",year,"_march_minus_oct_median_ndvi.tif",sep="")
writeRaster(dif_img,nameout)

}




