library(ncdf4)

met.fn.vec <- list.files('download/flux_anna/',full.names = T)

siteVpdHigh.fn <- c()

for (i in 1:2#seq_along(met.fn.vec)
     ) {
  nc.tmp <- nc_open(met.fn.vec[i])
  vpd.vec <- ncvar_get(nc.tmp,'VPD')
  
  vpd.high <- vpd.vec[vpd.vec>25]
  
  if(length(vpd.high)>100){
    siteVpdHigh.fn[i] <- met.fn.vec[i]
  }else{
    siteVpdHigh.fn[i] <- NA
  }
  
}

