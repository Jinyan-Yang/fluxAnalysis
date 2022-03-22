library(ncdf4)

met.fn.vec <- list.files('download/flux_anna/',full.names = T,include.dirs = FALSE)

siteVpdHigh.fn <- c()

for (i in seq_along(met.fn.vec)) {
  nc.tmp <- try(nc_open(met.fn.vec[i]))
  
  if(class(nc.tmp)!='try-error'){
    vpd.vec <- ncvar_get(nc.tmp,'VPD')
    if(max(vpd.vec,na.rm=T)>10){
      vpd.high <- vpd.vec[vpd.vec>25]
    }else{
      vpd.high <- vpd.vec[vpd.vec>2.5]
    }
    
    
    if(length(vpd.high)>1000){
      siteVpdHigh.fn[i] <- met.fn.vec[i]
    }else{
      siteVpdHigh.fn[i] <- NA
    }
  }

}
siteVpdHigh.fn <- siteVpdHigh.fn[!is.na(siteVpdHigh.fn)]
# 
file.copy(from = siteVpdHigh.fn,
          to = gsub(pattern = 'download/flux_anna/',
                    replacement = 'download/flux_anna/selected/',
                    siteVpdHigh.fn))
