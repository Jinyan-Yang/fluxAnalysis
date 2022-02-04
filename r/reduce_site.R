library(ncdf4)

met.fn.vec <- list.files('download/flux_anna/',full.names = T)

siteVpdHigh.fn <- c()

for (i in seq_along(met.fn.vec)) {
  nc.tmp <- nc_open(met.fn.vec[i])
  vpd.vec <- ncvar_get(nc.tmp,'VPD')
  
  vpd.high <- vpd.vec[vpd.vec>25]
  
  if(length(vpd.high)>100){
    siteVpdHigh.fn[i] <- met.fn.vec[i]
  }else{
    siteVpdHigh.fn[i] <- NA
  }
  
}
siteVpdHigh.fn <- siteVpdHigh.fn[!is.na(siteVpdHigh.fn)]
# 
file.copy(from = siteVpdHigh.fn,
          to = gsub(pattern = 'download/flux_anna/',
                    replacement = 'download/flux_anna/selected/',
                    siteVpdHigh.fn))
