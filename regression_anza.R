
require(raster)
library(snow)


############ includes paralelization  ##################

#rasterOptions(maxmemory = 3e+09)
#rasterOptions(chunksize = 1e+08)

#library(doParallel)  #Foreach Parallel Adaptor 
#library(foreach)     #Provides foreach looping construct
#UseCores <- detectCores() -1
#cl       <- makeCluster(UseCores)
#registerDoParallel(cl)
#foreach(i=1:length(stack_list)) %dopar% {
#  library(raster)
#  ...
#}
#stopCluster(cl)


### annalize prism data
exam=raster("G:/annual/1984summer_ndvi.tif")
list_rec2=list.files("C:/projects/anza-borrego/june_sep/",pattern = ".tif$", recursive = TRUE, full.names=T)
year=1981

for (i in 1:36){
year=year+1
sum_dr=raster(list_rec2[i])
#sum_dr = sum_dr/12
temp2=resample(sum_dr,exam,method="bilinear")
outname=paste("G:/prism/temp/30m_june_sep/",year,"_oct_sep.tif",sep="")
writeRaster(temp2,outname, fomat="GTiff",overwrite=T)

}


pas1=0
  pas2=0
len = length(list_rec2)
steps = 36
year=1981
mod=1
model=1
for (i in 1:steps){
year=year+1
     pas1=(i*12)-2
   pas2= pas1+11
   dr = stack(list_rec2[pas1:pas2])
  sum_dr=sum(dr)
 
  temp2=resample(sum_dr,exam,method="bilinear")
   outname=paste("G:/prism/precip/30m_res_seasonal/",year,"_oct_sep.tif",sep="")
  writeRaster(temp2,outname, fomat="GTiff",overwrite=T)
  
}


############# calculate regression  #####################
list_rec1=list.files("G:/annual_june_sept/",pattern = "ndvi.tif$", recursive = TRUE, full.names=T)
list_rec2=list.files("G:/prism/precip/30m_res_seasonal/",pattern = ".tif$", recursive = TRUE, full.names=T)
list_rec3=list.files("G:/prism/temp/30m_res_seasonal/",pattern = ".tif$", recursive = TRUE, full.names=T)

list_rec1=list.files("G:/annual_june_sept/",pattern = "ndvi.tif$", recursive = TRUE, full.names=T)
list_rec2=list.files("G:/prism/precip/30m_oct_mar/",pattern = ".tif$", recursive = TRUE, full.names=T)
list_rec3=list.files("G:/prism/temp/30m_june_sep/",pattern = ".tif$", recursive = TRUE, full.names=T)


s1=stack(list_rec1)
s2=stack(list_rec3[3:36])

s <- stack(s1, s2)


#fun2 <- function(x) {if (any(is.na(x))){NA} else {model <- summary(lm(x[1:34] ~ x[35:68])); c(model$coefficients[2], model$r.squared) }}
fun2 <- function(x) {
  if (any(is.na(x))){
    return(c(NA,NA))
  } else {
    model <- summary(lm(x[2:34] ~ x[36:68]))
    if (is.numeric(model$coefficients[2,1]) & is.numeric(model$r.squared)){
      return(c(model$coefficients[2,1], model$r.squared))
    }else{
      return(c(NA,NA))
    }
  }}

beginCluster(6)

x2 <- clusterR(s, calc, args=list(fun2))

endCluster()

slope = x2[[1]]
rsquare = x2[[2]]
writeRaster(slope,"C:/projects/anza-borrego/precip_reg_results/winter_precip_summer_ndvi_temp_slope.tif", overwrite = T)
writeRaster(rsquare,"C:/projects/anza-borrego/precip_reg_results/winter_precip_summer_ndvi_temp_rsquare.tif", overwrite = T)





s1=stack(list_rec1)
s2=stack(list_rec2[3:36])

s <- stack(s1, s2)


#fun2 <- function(x) {if (any(is.na(x))){NA} else {model <- summary(lm(x[1:34] ~ x[35:68])); c(model$coefficients[2], model$r.squared) }}
fun2 <- function(x) {
  if (any(is.na(x))){
    return(c(NA,NA))
  } else {
      model <- summary(lm(x[2:34] ~ x[36:68]))
      if (is.numeric(model$coefficients[2,1]) & is.numeric(model$r.squared)){
        return(c(model$coefficients[2,1], model$r.squared))
      }else{
        return(c(NA,NA))
      }
     }}

beginCluster(6)

x2 <- clusterR(s, calc, args=list(fun2))

endCluster()

slope = x2[[1]]
rsquare = x2[[2]]
writeRaster(slope,"C:/projects/anza-borrego/precip_reg_results/winter_precip_summer_ndvi_precip_slope.tif", overwrite = T)
writeRaster(rsquare,"C:/projects/anza-borrego/precip_reg_results/winter_precip_summer_ndvi_precip_rsquare.tif", overwrite = T)



s1=stack(list_rec1)
s2=stack(list_rec2[3:36])

s <- stack(s1, s2)


#fun2 <- function(x) {if (any(is.na(x))){NA} else {model <- summary(lm(x[1:34] ~ x[35:68])); c(model$coefficients[2], model$r.squared) }}
fun2 <- function(x) {
  if (any(is.na(x))){
    return(c(NA,NA,NA))
  } else {
    model <- summary(lm(x[2:15] ~ x[36:49]))
    if (is.numeric(model$coefficients[2,1]) & is.numeric(model$r.squared)){
      return(c(model$coefficients[2,1], model$r.squared, model$coefficients[2,4]))
    }else{
      return(c(NA,NA,NA))
    }
  }}

beginCluster(6)

x2 <- clusterR(s, calc, args=list(fun2))

endCluster()

slope = x2[[1]]
rsquare = x2[[2]]
p=x2[[3]]
writeRaster(slope,"C:/projects/anza-borrego/precip_reg_results/winter_precip_summer_ndvi1985-1998_precip_slope.tif", overwrite = T)
writeRaster(rsquare,"C:/projects/anza-borrego/precip_reg_results/winter_precip_summer_ndvi1985-1998_precip_rsquare.tif", overwrite = T)
writeRaster(p,"C:/projects/anza-borrego/precip_reg_results/winter_precip_summer_ndvi1985-1998_precip_p.tif", overwrite = T)



s1=stack(list_rec1[16:34])
s2=stack(list_rec2[17:36])

s <- stack(s1, s2)


#fun2 <- function(x) {if (any(is.na(x))){NA} else {model <- summary(lm(x[1:34] ~ x[35:68])); c(model$coefficients[2], model$r.squared) }}
fun2 <- function(x) {
  if (any(is.na(x))){
    return(c(NA,NA,NA))
  } else {
    model <- summary(lm(x[1:19] ~ x[21:39]))
    if (is.numeric(model$coefficients[2,1]) & is.numeric(model$r.squared)){
      return(c(model$coefficients[2,1], model$r.squared,model$coefficients[2,4]))
    }else{
      return(c(NA,NA,NA))
    }
  }}

beginCluster(6)

x2 <- clusterR(s, calc, args=list(fun2))

endCluster()

#x2 <- calc(s, fun2)
slope = x2[[1]]
rsquare = x2[[2]]
p=x2[[3]]
writeRaster(slope,"C:/projects/anza-borrego/precip_reg_results/winter_precip_summer_ndvi1999-2017_precip_slope2.tif", overwrite = T)
writeRaster(rsquare,"C:/projects/anza-borrego/precip_reg_results/winter_precip_summer_ndvi1999-2017_precip_rsquare2.tif", overwrite = T)
writeRaster(p,"C:/projects/anza-borrego/precip_reg_results/winter_precip_summer_ndvi1999-2017_precip_p.tif", overwrite = T)



#plot(rsquare)

s1=stack(list_rec1)
s2=stack(list_rec2[2:36])
s <- stack(s1, s2)

#fun2 <- function(x) {if (any(is.na(x))){NA} else {model <- summary(lm(x[1:34] ~ x[36:69] + x[35:68]));model$r.squared }}
fun2 <- function(x) {
  if (any(is.na(x))){
    return(c(NA,NA,NA))
  } else {
    model <- summary(lm(x[2:34] ~ x[37:69] + x[36:68]))
    if (is.numeric(model$coefficients[2]) & is.numeric(model$r.squared)){
      return(c(model$r.squared,model$coefficients[2],model$coefficients[3]))
    }else{
      return(c(NA,NA,NA))
    }
  }}
beginCluster(6)

x3 <- clusterR(s, calc, args=list(fun2))

endCluster()

ras1=x3[[1]]
ras2=x3[[2]]
ras3=x3[[3]]

writeRaster(ras1,"C:/projects/anza-borrego/precip_reg_results/winter_precip_summer_ndvi_precip_precip1lag_rsquare2.tif", overwrite = T)
writeRaster(ras2,"C:/projects/anza-borrego/precip_reg_results/winter_precip_summer_ndvi_precip_precip1lag_slope_precip2.tif", overwrite = T)
writeRaster(ras3,"C:/projects/anza-borrego/precip_reg_results/winter_precip_summer_ndvi_precip_precip1lag_slope_lag2.tif", overwrite = T)



s1=stack(list_rec1)
s2=stack(list_rec2[2:36])
s3=stack(list_rec3[4:36])
s <- stack(s1, s2,s3)

rrr=1
res1=1
model=1
ttes=1
test1=0
mod=1
dr=c(1:33)
#fun2 <- function(x) {if (any(is.na(x))){NA} else {model <- summary(lm(x[1:34] ~ x[36:69] + x[35:68]));model$r.squared }}
fun2 <- function(x) {
  if (any(is.na(x))){
    return(c(NA,NA,NA,NA,NA,NA,NA))
  } else {
    
    mod=lm(x[2:34] ~ x[37:69] + x[36:68] + x[70:102])
    model <- summary(mod)
  
    
    if (is.numeric(model$coefficients[2]) & is.numeric(model$r.squared)){
      res1 =   unlist(resid(mod))
      
      dr=c(1:33)
      test1 =(cbind(res1,dr))
      rrr = summary(lm(test1[,1] ~test1[,2]))
      ttes = t.test(res1[1:14],res1[15:33])$p.value
      return(c(model$r.squared,model$coefficients[2],model$coefficients[3],model$coefficients[4],rrr$coefficients[2,4],ttes,rrr$coefficients[2,1]))
    }else{
      return(c(NA,NA,NA,NA,NA,NA,NA))
    }
  }}

beginCluster(6)

x4 <- clusterR(s, calc, args=list(fun2))

endCluster()

#x4 <- calc(s, fun2)

#writeRaster(x4,"C:/projects/anza-borrego/precip_reg_results/summer_ndvi_precip_precip1lag_temp_rsquare.tif", overwrite = T)
ras1=x4[[1]]
ras2=x4[[2]]
ras3=x4[[3]]
ras4=x4[[4]]
ras5=x4[[5]]
ras6=x4[[6]]
ras7=x4[[7]]
writeRaster(ras1,"C:/projects/anza-borrego/precip_reg_results/winter_precip_summer_ndvi_precip_precip1lagtemp_rsquare2.tif", overwrite = T)
writeRaster(ras2,"C:/projects/anza-borrego/precip_reg_results/winter_precip_summer_ndvi_precip_precip1lagtemp_slope_precip2.tif", overwrite = T)
writeRaster(ras3,"C:/projects/anza-borrego/precip_reg_results/winter_precip_summer_ndvi_precip_precip1lagtemp_slope_lag2.tif", overwrite = T)
writeRaster(ras4,"C:/projects/anza-borrego/precip_reg_results/winter_precip_summer_ndvi_precip_precip1lagtemp_slope_temp2.tif", overwrite = T)
writeRaster(ras5,"C:/projects/anza-borrego/precip_reg_results/winter_precip_summer_ndvi_precip_precip1lagtemp_slope_residu_p.tif", overwrite = T)
writeRaster(ras6,"C:/projects/anza-borrego/precip_reg_results/winter_precip_summer_ndvi_precip_precip1lagtemp_slope_ttest_p.tif", overwrite = T)
writeRaster(ras7,"C:/projects/anza-borrego/precip_reg_results/winter_precip_summer_ndvi_precip_precip1lagtemp_slope_residu_slope.tif", overwrite = T)






s1=stack(list_rec1)
s2=stack(list_rec2[2:36])
s <- stack(s1, s2)

#fun2 <- function(x) {if (any(is.na(x))){NA} else {model <- summary(lm(x[1:34] ~ x[36:69] + x[35:68]));model$r.squared }}
fun2 <- function(x) {
  if (any(is.na(x))){
    return(NA)
  } else {
    model <- summary(lm(x[2:34] ~ x[37:69] + x[1:33]))
    if (is.numeric(model$coefficients[2]) & is.numeric(model$r.squared)){
      return(model$r.squared)
    }else{
      return(NA)
    }
  }}
#x3 <- calc(s, fun2)
beginCluster(6)

x3 <- clusterR(s, calc, args=list(fun2))

endCluster()
writeRaster(x3,"C:/projects/anza-borrego/precip_reg_results/winter_precip_summer_ndvi_precip_NDVI1lag_rsquare.tif", overwrite = T)



s1=stack(list_rec1)
s2=stack(list_rec2[3:36])
s3=stack(list_rec3[3:36])
s <- stack(s1, s2,s3)

#fun2 <- function(x) {if (any(is.na(x))){NA} else {model <- summary(lm(x[1:34] ~ x[36:69] + x[35:68]));model$r.squared }}
fun2 <- function(x) {
  if (any(is.na(x))){
    return(NA)
  } else {
    model <- summary(lm(x[2:34] ~ x[36:68] + x[70:102]))
    if (is.numeric(model$coefficients[2]) & is.numeric(model$r.squared)){
      return(model$r.squared)
    }else{
      return(NA)
    }
  }}
beginCluster(6)

x3 <- clusterR(s, calc, args=list(fun2))

endCluster()

writeRaster(x3,"C:/projects/anza-borrego/precip_reg_results/winter_precip_summer_ndvi_precip_temp_rsquare.tif", overwrite = T)


s1=stack(list_rec1)
s2=stack(list_rec2[3:36])
s3=stack(list_rec3[3:36])
s <- stack(s1, s2,s3)

#fun2 <- function(x) {if (any(is.na(x))){NA} else {model <- summary(lm(x[1:34] ~ x[36:69] + x[35:68]));model$r.squared }}
fun2 <- function(x) {
  if (any(is.na(x))){
    return(NA)
  } else {
    model <- summary(lm(x[2:34] ~ x[36:68] + x[70:102] + x[1:33]))
    if (is.numeric(model$coefficients[2]) & is.numeric(model$r.squared)){
      return(model$r.squared)
    }else{
      return(NA)
    }
  }}
beginCluster(6)

x3 <- clusterR(s, calc, args=list(fun2))

endCluster()

writeRaster(x3,"C:/projects/anza-borrego/precip_reg_results/winter_precip_summer_ndvi_precip_ndvi1lag_temp_rsquare.tif", overwrite = T)



list_rec1=list.files("G:/monthly/",pattern = "06median_ndvi.tif$", recursive = TRUE, full.names=T)

s1=stack(list_rec1)
s2=stack(list_rec2[3:36])
s3=stack(list_rec3[3:36])
s <- stack(s1, s2,s3)

fun2 <- function(x) {
  if (any(is.na(x))){
    return(c(NA,NA))
  } else {
    model <- summary(lm(x[1:34] ~ x[35:68]))
    if (is.numeric(model$coefficients[2,1]) & is.numeric(model$r.squared)){
      return(c(model$coefficients[2,1], model$r.squared))
    }else{
      return(c(NA,NA))
    }
  }}

beginCluster(6)

x2 <- clusterR(s, calc, args=list(fun2))

endCluster()

slope = x2[[1]]
rsquare = x2[[2]]
writeRaster(slope,"C:/projects/anza-borrego/precip_reg_results/winter_precip_june_ndvi_precip_slope.tif")
writeRaster(rsquare,"C:/projects/anza-borrego/precip_reg_results/winter_precip_june_ndvi_precip_rsquare.tif")


