


install.packages('e1071', dependencies=TRUE)
library(raster)
library(doParallel)  #Foreach Parallel Adaptor
library(foreach)     #Provides foreach looping construct
library("randomForest")
library("RStoolbox")
library("rgdal")
UseCores <- detectCores() -1

 dir1="H:/rapideye/"

imgs = list.files(dir1,recursive = TRUE, full.names=T, pattern = "Analytic.tif")
 i=1
for(i in 1:(length(imgs)/2)){
p=(i*2)-1
red1=raster(imgs[p],band=3)
red2=raster(imgs[p+1],band=3) 
nir1=raster(imgs[p],band=5)
nir2=raster(imgs[p+1],band=5)

ndvi1=((nir1-red1)/(nir1+red1))
ndvi2=((nir2-red2)/(nir2+red2))

ndvi = merge(ndvi1,ndvi2)

name = paste(dir1,i,"_ndvi",".tif",sep="")
writeRaster(ndvi,name, overwrite =T)
}

ndvis= list.files(dir1,recursive = TRUE, full.names=T, pattern = "_ndvi.tif")
nam = list.files(dir1,recursive = TRUE, full.names=F, pattern = "_ndvi.tif")
stack_ndvi = stack(ndvis)
mean_ndvi = mean(stack_ndvi)

writeRaster(mean_ndvi,"H:/rapideye/mean_ndvi.tif")
p=1
for(p in 1:length(ndvis)){
  
ndvi1 = raster(ndvis[p])
rel_ndvi = (ndvi1-mean_ndvi)/mean_ndvi

name = paste(dir1, "rel_",nam[p],sep="")
writeRaster(rel_ndvi,name, overwrite =T)
}

################# clasification carrizo gorge #############################


im = list.files("H:/rapideye/class/",recursive = TRUE, full.names=T, pattern = "_ndvi.tif")
rel_stack = stack(im)


#ref_list = list.files("C:/projects/anza-borrego/tamarix_ref/",recursive = TRUE, full.names=T, pattern = ".shp$")

bare=readOGR("C:/projects/anza-borrego/tamarix_ref/CarrizoGorgeBareGandRock_PointData.shp")
car_veg=readOGR("C:/projects/anza-borrego/tamarix_ref/CarrizoGorgeVeg_PointData.shp")
tam=readOGR("C:/projects/anza-borrego/tamarix_ref/TamariskPoints_UpperCG.shp")
notam=readOGR("C:/projects/anza-borrego/tamarix_ref/no_tamarix.shp")


bare = spTransform(bare, crs(rel_stack))
car_veg = spTransform(car_veg, crs(rel_stack))
tam = spTransform(tam, crs(rel_stack))
notam = spTransform(notam, crs(rel_stack))

car_veg=car_veg[car_veg$CoverType %in% names(which(table(car_veg$CoverType) > 5)), ]
bare$CoverType = "bare"

nottam=tam[tam$Species != "Tamarisk",]
tam=tam[tam$Species == "Tamarisk",]
nottam$CoverType = "other"
tam$CoverType = "Tamram"
notam$CoverType = "other"

bare1=bare[,("CoverType")]
car_veg1= car_veg[,"CoverType"]
tam1 = tam[,"CoverType"]
nottam1 = nottam[,"CoverType"]
notam1 = notam[,"CoverType"]
ref =rbind(bare1,car_veg1,tam1,nottam1,notam1)

clas = superClass(rel_stack,trainData=ref,tuneLength = 10,nsamples = 5,mode='classification',responseCol = "CoverType" , model="rf", na.omit=T)

writeRaster(clas$map,"C:/projects/anza-borrego/test_carrizo_clas2.tif",overwrite=T)


ref1 = ref
ref1$tam[ref1$CoverType == "Tamram"] = 1
ref1$tam[ref1$CoverType != "Tamram"] = 0

ref1 = ref1[,"tam"]

clas_prob = superClass(rel_stack,trainData=ref1,trainPartition = 0.8, tuneLength = 10,nsamples = 250,mode='regression',responseCol = "tam" , model="rf", na.omit=T)

writeRaster(clas_prob$map,"C:/projects/anza-borrego/test_carrizo_prob_clas2.tif",overwrite=T)


############# dont use lower reference

ref =rbind(bare1,car_veg1)
clas = superClass(rel_stack,trainData=ref,tuneLength = 10,nsamples = 5,mode='classification',responseCol = "CoverType" , model="rf", na.omit=T)

writeRaster(clas$map,"C:/projects/anza-borrego/test_carrizo_clas3_only_vegtype.tif",overwrite=T)



clas$map@data

clas_prob$map@data

