



library(raster)
library(rgdal)
install.packages('e1071', dependencies=TRUE)
library("randomForest")
library("RStoolbox")
library("rgdal")
library("rgeos")

library(foreach)
library("doSNOW")

im = list.files("F:/monthly/",recursive = TRUE, full.names=T, pattern = "_ndvi.tif$")
#2014 = [358:369]
#2015 = [370:381]
#2016 = [382:393]
rel_stack = stack(im[358:369])
l = c()
p=382
for (p in 358:369){
  mr = (raster(im[(p-1)]) + raster(im[(p+1)]))/2
  mrp = raster(im[p])
  mrp2 = cover(mrp,mr)
  l=c(l,mrp2)
}

rel_stack=stack(l)
me =mean(rel_stack)

rel_stack2 = rel_stack/me
rel_stack = stack(rel_stack2,rel_stack,me)

#ref_list = list.files("C:/projects/anza-borrego/tamarix_ref/",recursive = TRUE, full.names=T, pattern = ".shp$")

bare=readOGR("C:/projects/anza-borrego/tamarix_ref/CarrizoGorgeBareGandRock_PointData.shp")
car_veg=readOGR("C:/projects/anza-borrego/tamarix_ref/CarrizoGorgeVeg_PointData.shp")
tam=readOGR("C:/projects/anza-borrego/tamarix_ref/TamariskPoints_UpperCG.shp")
notam=readOGR("C:/projects/anza-borrego/tamarix_ref/no_tamarix.shp")


bare = spTransform(bare, crs(rel_stack))
car_veg = spTransform(car_veg, crs(rel_stack))
tam = spTransform(tam, crs(rel_stack))
notam = spTransform(notam, crs(rel_stack))

tam = tam[sample(1:length(tam),250),] #extract only 250 points from this dataset to avoid overfitting.

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

#clas = superClass(rel_stack,trainData=ref,tuneLength = 10,nsamples = 5,mode='classification',responseCol = "CoverType" , model="rf", na.omit=T)

#writeRaster(clas$map,"C:/projects/anza-borrego/test_carrizo_clas2.tif",overwrite=T)


ref1 = ref
ref1$tam[ref1$CoverType == "Tamram"] = 1
ref1$tam[ref1$CoverType != "Tamram"] = 0

ref1 = ref1[,"tam"]

#clas_prob = superClass(rel_stack,trainData=ref1,trainPartition = 0.8, tuneLength = 10,nsamples = 250,mode='regression',responseCol = "tam" , model="rf", na.omit=T)

#writeRaster(clas_prob$map,"C:/projects/anza-borrego/test_carrizo_prob_clas_landsat_v2.tif",overwrite=T)

#v <- as.data.frame(extract(rel_stack, ref1))



####################### seperate trail


# CREATE RASTER STACK
xvars <- rel_stack     

# READ POINT SHAPEFILE TRAINING DAT
sdata <- ref1


# ASSIGN RASTER VALUES TO TRAINING DATA
v <- as.data.frame(extract(xvars, sdata))
sdata@data = data.frame(sdata@data, v[match(rownames(sdata@data), rownames(v)),])

sdata2014=sdata
sdata2015=sdata
sdata2016=sdata

sdata_f = rbind(sdata2014,sdata2015,sdata2016)#sdata comes from script below
# polygon based samples added below

sdata_f1=sdata_f
############################## extract point data from polygons




ta =readOGR("C:/projects/anza-borrego/tamarix_ref/CDD_Tamarisk_Teale_Subset.shp")
nota_pol = readOGR("C:/projects/anza-borrego/tamarix_ref/no_tamarix_pol.shp")
ref1 = spTransform(ta, crs(rel_stack))
nota_pol = spTransform(nota_pol, crs(rel_stack))
ref1$tam = 1
nota_pol$tam = 0

steps = (unique(ref1$YEAR))
m_year = min(steps)

sdata_final = sdata_f@data
for (kr in 2010:2010){
  
  ref2=ref1[ref1$YEAR>(kr),] 
  ref2=ref2[ref2$YEAR<(kr+3),]
  
  no_ref = ref1[ref1$YEAR2<(kr),]
  no_ref = no_ref[no_ref$YEAR2>(kr-3),]
  no_ref2 = nota_pol[(kr-1) > nota_pol$YEAR,]
  
  
  st =  ((kr-1985)*12)+10
  st1 = st+11
  
  im = list.files("F:/monthly/",recursive = TRUE, full.names=T, pattern = "_ndvi.tif$")
  #
  #2015 = [370:381]
  #2016 = [382:393]
  rel_stack = stack(im[st:st1])
  l = c()
  p=382
  for (p in st:st1){
    mr = (raster(im[(p-1)]) + raster(im[(p+1)]))/2
    mrp = raster(im[p])
    mrp2 = cover(mrp,mr)
    l=c(l,mrp2)
  }
  
  rel_stack=stack(l)
  me =mean(rel_stack)
  
  rel_stack2 = rel_stack/me
  rel_stack = stack(rel_stack2,rel_stack,me)
  
  
  # CREATE RASTER STACK
  xvars <- rel_stack     
  
  # READ POINT SHAPEFILE TRAINING DAT
  
  if (length(ref2$tam)>0){
    ref3=aggregate(ref2,"tam")
    sdata <- ref3
    
    # ASSIGN RASTER VALUES TO TRAINING DATA
    v <- as.data.frame(extract(xvars, sdata))
    v$tam = 1
    sdata= cbind(v$tam,v[,1:25])
    names(sdata)[1] = "tam"
    
    sdata_final = rbind(sdata_final,sdata)
  }
  
  if (length(no_ref$tam) > 0){
    no_ref$tam = 0
    no_ref = aggregate(no_ref,"tam")
    
    vp <- as.data.frame(extract(xvars, no_ref))
    vp$tam = 0
    prrr = cbind(vp$tam,vp[,1:25])
    names(prrr)[1] = "tam"
    
    sdata_final = rbind(sdata_final,prrr)
  }
  
 # if (length(no_ref2$tam) > 0){
  #  no_ref2$tam = 0
  #  no_ref2 = aggregate(no_ref2,"tam")
  #  
  #  vp <- as.data.frame(extract(xvars, no_ref2))
  #  vp$tam = 0
  #  prrr = cbind(vp$tam,vp[,1:25])
  #  names(prrr)[1] = "tam"
    
  #  sdata_final = rbind(sdata_final,prrr)
  #}
}

################### run random forest  ################################

sdata_final=na.omit(sdata_final)
#sdata_final1 = sdata_final[sample(nrow(sdata_final),5000), ]
#sdata_final2= sdata_final
#sdata_final = rbind(sdata_final1, sdata_f1@data)

# RUN RF MODEL
rf.mdl <- randomForest(x=sdata_final[,2:ncol(sdata_final)], y=sdata_final[,"tam"],
                       ntree=500, importance=TRUE)

registerDoSNOW(makeCluster(7, type="SOCK"))
rf.mdl <- foreach(ntree=rep(60, 7), .combine=combine, .packages='randomForest') %dopar% randomForest(x=sdata_final[,2:ncol(sdata_final)], y=sdata_final[,"tam"], importance=TRUE, ntree = ntree)

# CHECK ERROR CONVERGENCE
plot(rf.mdl)
plot(rf)
# PLOT mean decrease in accuracy VARIABLE IMPORTANCE
varImpPlot(rf.mdl, type=1)
varImpPlot(rf, type=1)
# PREDICT MODEL

for (year in 2000:2000){
  
  st =  ((year-1985)*12)+10
  st1 = st+11
  
  im = list.files("F:/monthly/",recursive = TRUE, full.names=T, pattern = "_ndvi.tif$")
  #
  #2015 = [370:381]
  #2016 = [382:393]
  rel_stack = stack(im[st:st1])
  l = c()
  p=382
  for (p in st:st1){
    mr = (raster(im[(p-1)]) + raster(im[(p+1)]))/2
    mrp = raster(im[p])
    mrp2 = cover(mrp,mr)
    l=c(l,mrp2)
  }
  
  rel_stack=stack(l)
  me =mean(rel_stack)
  
  rel_stack2 = rel_stack/me
  rel_stack = stack(rel_stack2,rel_stack,me)
  
  
  
  rps = predict(rel_stack, rf.mdl, type="response", 
                index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
  
  #plot(rps)
  out = paste("C:/projects/anza-borrego/RR_annual_landsat_tam2/",year,"_ABDSP_tamarix_probability.tif",sep="")
  writeRaster(rps,out,overwrite=T)
}



