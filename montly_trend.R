
library(raster)
library(LSD)
library(rgdal)
library(R.utils)

list_rec=list.files("F:/prism/temp/clip/",pattern = ".tif$", recursive = TRUE, full.names=T)

lm_fun = function(x) {if (all(is.na(x))){NA} else {m = lm(x ~ tim); summary(m)$coefficients[2] }} # slope

precip_s1=raster::stack(list_rec)
#precip_s3=raster::brick(precip_s1)
nr_timestep=length(list_rec)
tim=(1:nr_timestep)

annual_ave_stack.slope= calc(precip_s1, lm_fun)
#annual_ave_stack.slope=annual_ave_stack.slope
writeRaster(annual_ave_stack.slope,"F:/prism/trend_temperature.tif")




list_rec2=list.files("F:/monthly/",pattern = "03median", recursive = TRUE, full.names=T)
ndvi=raster::stack(list_rec)

mean_ndvi=mean(ndvi, na.rm=TRUE)
mean_ndvi_zero=abs(mean_ndvi)
relative_ndvi_change=(annual_ave_stack.slope/mean_ndvi_zero)*100

writeRaster(relative_ndvi_change,"F:/relative_march_minsummer_change_ndvi.tif", overwrite=T)
