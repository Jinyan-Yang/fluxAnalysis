library(ncdf4)
# read flies
file.met.vec <- list.files(path = 'download/flux_anna/selected/',
                           full.names = T,pattern = 'Met.nc')
file.flux.vec <- list.files(path = 'download/flux_anna/selected/',
                           full.names = T,pattern = 'Flux.nc')
# loop through all sites
hiD.period.ls <- list()

for(i in seq_along(file.met.vec)){
  # met
  nc.met <- nc_open(file.met.vec[i])
  
  start.time <- (nc.met$dim$time$units)
  start.time <- gsub('seconds since ','',start.time)
  

  # 
  met.df <- data.frame(Site = substr(file.met.vec[i],29,34),
                       DateTime = as.POSIXlt( ncvar_get(nc.met,'time'),
                                          origin = start.time,tz = 'GMT'),
                       lat = ncvar_get(nc.met,'latitude'),
                       lon = ncvar_get(nc.met,'longitude'),
                       VPD = ncvar_get(nc.met,'VPD') ,
                       LAI = ncvar_get(nc.met,'LAI'),
                       PPT = ncvar_get(nc.met,'Precip'))
  met.df$Date <- as.Date(met.df$DateTime)
  met.df$PPT_mm_30min <- met.df$PPT * 1800
  
  # vpd unit covert
  if(max(met.df$VPD) >10){
    vpd.conv = 0.1
  }else{
    vpd.conv = 1
  }
  
  # get past rainfall
  all.date.vec <- unique(met.df$Date)
  for(date.i in seq_along(all.date.vec)){
    
    past.rain.vec <- met.df$PPT_mm_30min[met.df$Date<= all.date.vec[date.i] &
                                  met.df$Date >= (all.date.vec[date.i] - 30)]
    if(length(past.rain.vec)<30*48){
      met.df$past.30.rain.mm[met.df$Date == all.date.vec[date.i]] <- NA
    }else{
      met.df$past.30.rain.mm[met.df$Date == all.date.vec[date.i]] <- sum(past.rain.vec) 
    }
  }
  
  # 
  met.df$vpd_kPa <- met.df$VPD *vpd.conv

  # # 
  # met.df$Date <- as.Date(met.df$Date)
  # highD.date <- met.df$Date[met.df$vpd_kPa>3]
  # mon.select <- format(unique(highD.date),'%Y-%m')
  # met.sub.df <- met.df[format(met.df$Date,'%Y-%m') %in% mon.select,]
  

  # flux
  nc.flux <- nc_open(file.flux.vec[i])
  
  start.time <- (nc.flux$dim$time$units)
  start.time <- gsub('seconds since ','',start.time)
  # some sites do not have Qg
  qg.vec <- try(ncvar_get(nc.flux,'Qg'))
   
  if(class(qg.vec)=='try-error'){
    qg.vec <- NA
  }             
  # 
  flux.df <- data.frame(DateTime = as.POSIXlt(ncvar_get(nc.flux,'time'),
                                              origin = start.time,tz = 'GMT'),
                        veg_type = ncvar_get(nc.flux,'IGBP_veg_long'),
                        NEE = ncvar_get(nc.flux,'NEE'),
                        Rnet = ncvar_get(nc.flux,'Rnet'),
                        Qg = qg.vec,
                        Qh = ncvar_get(nc.flux,'Qh'),
                        Qle = ncvar_get(nc.flux,'Qle'))
  
  met.flux.df <- merge(met.df,flux.df)
  saveRDS(met.flux.df,paste0('cache/',substr(file.met.vec[i],29,34),
                             '.flux.met.rds'))
  
  hiD.period.ls[[i]] <- met.flux.df
  
}
#############
hiD.df <- do.call(rbind,hiD.period.ls)
#############
tmp.df <- hiD.period.ls[[5]]
tmp.df <- tmp.df[tmp.df$Rnet> 10,]
plot(c(Qle/Rnet)~vpd_kPa,data = tmp.df,ylim=c(0,1))

# x <- tmp.df$Rnet
# y <- with(tmp.df,c(Qle/Rnet))
# z <- kde2d(x, y, n = 50)
# contour(z, lwd = 2,
#         col = hcl.colors(15, "Spectral"),ylim=c(0,1))

plot(Qle~vpd_kPa,data = tmp.df)
plot(Qh~vpd_kPa,data = hiD.period.ls[[1]])
plot(c(Qle/Rnet)~vpd_kPa,data = tmp.df,ylim=c(0,1))



names(nc.met$var)
names(nc.flux$var)
nc.flux$var$Qle$units
nc.flux$var$Rnet$units
