

library(raster)

mask_anza=raster("/Users/stijnhantson/Documents/projects/anza_borrego/data/mask_anza.tif")
mask_sh = shapefile("/Users/stijnhantson/Documents/projects/anza_borrego/extra_mask.shp")
mean_p = raster("/Users/stijnhantson/Documents/projects/anza_borrego/results/mean_precip_30m.tif")
d_dem = raster("/Users/stijnhantson/Documents/projects/anza_borrego/DEM_anza/anza_dem_proj.tif")
left = shapefile("/Users/stijnhantson/Documents/projects/anza_borrego/temp/test_l.shp")

TA = crs(mask_anza)

left = spTransform(left,TA)

mask_sh=spTransform(mask_sh,TA)
mask_anza[mask_anza>0]=0
mask_anza[is.na(mask_anza)]=1
mask_anza = mask(mask_anza,mask_sh)
mask_anza[mask_anza==0]= NA
mask_anza[ is.na(mean_p)] = NA


plot(d_dem)

dd = d_dem
dd[] = NA

dd[d_dem >= 500 & d_dem < 1500] = 1
dd=dd/mask_anza


dd1[] = NA
dd1[d_dem < 500] = 1
dd1 = mask(dd1,left)

list_rec=list.files("/Users/stijnhantson/Documents/projects/anza_borrego/annual_june_sept/",pattern = "summer_ndvi.tif$", recursive = TRUE, full.names=T)

desert = 0
mountain = 0
for (ps in 1:length(list_rec)){
  ndvi = raster(list_rec[ps])
  tr1=mask(ndvi,dd)
  tr2=mask(ndvi,dd1)
  
  desert[ps] = mean(tr2,na.omit=T)
  mountain[ps] = mean(tr1,na.omit=T)
}

plot(dd)

dd_pol=rasterToPolygons(dd, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=FALSE)

