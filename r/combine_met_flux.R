library(ncdf4)
# read flies
file.met.vec <- list.files(path = 'download/flux_anna/selected/',
                           full.names = T,pattern = 'Met.nc')
file.flux.vec <- list.files(path = 'download/flux_anna/selected/',
                            full.names = T,pattern = 'Flux.nc')
# loop through all sites

for(i in seq_along(file.met.vec)){
  
  out.nm <- paste0('cache/',substr(file.met.vec[i],29,34),
                   '.flux.met.rds')
  
  if(!file.exists(out.nm)){
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
                         Tair = ncvar_get(nc.met,'Tair') ,
                         RH = ncvar_get(nc.met,'RH') ,
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
                          Qle = ncvar_get(nc.flux,'Qle'),
                          Qle.qc = ncvar_get(nc.flux,'Qle_qc'))
    # names(nc.flux$var)
    met.flux.df <- merge(met.df,flux.df)
    saveRDS(met.flux.df,out.nm)
    
    # hiD.period.ls[[i]] <- met.flux.df
  }
  
  
}


# process alice spring###
library(ncdf4)
fn.vec <- list.files('download/asm/',pattern = '.nc',full.names = T)

# 
asm.ls <- list()
for (i in seq_along(fn.vec)) {
  
  nc.flux <- nc_open(fn.vec[i])
  start.time <- (nc.flux$dim$time$units)
  start.time <- gsub('days since ','',start.time)
  # names(nc.flux$var$Fco2)
  # nc.flux$var$Fe
  qg.vec <- try(ncvar_get(nc.flux,'Fg'))
  met.df <- data.frame(Site = 'AU-ASM',
                       DateTime = as.POSIXlt( ncvar_get(nc.flux,'time')*24*3600,
                                              origin = start.time,tz = 'GMT'),
                       lat = ncvar_get(nc.flux,'latitude'),
                       lon = ncvar_get(nc.flux,'longitude'),
                       VPD = ncvar_get(nc.flux,'VPD') ,
                       Tair = ncvar_get(nc.flux,'Ta') ,
                       RH = ncvar_get(nc.flux,'RH') ,
                       LAI = NA,
                       PPT = ncvar_get(nc.flux,'Precip'),
                       veg_type = 'Evergreen Needleleaf Forests',
                       NEE = ncvar_get(nc.flux,'Fco2'),
                       Rnet = ncvar_get(nc.flux,'Fn'),
                       Qg = qg.vec,
                       Qh = ncvar_get(nc.flux,'Fh'),
                       Qle = ncvar_get(nc.flux,'Fe'),
                       Qle.qc = ncvar_get(nc.flux,'Fe_QCFlag'))
  
  met.df$Date <- as.Date(met.df$DateTime)
  met.df$PPT[met.df$PPT<0] <- 0
  met.df$PPT_mm_30min <- met.df$PPT #* 1800
  
  # vpd unit covert
  if(max(met.df$VPD) >10){
    vpd.conv = 0.1
  }else{
    vpd.conv = 1
  }
  met.df$vpd_kPa <- met.df$VPD * vpd.conv
  # get past rainfall
  all.date.vec <- unique(met.df$Date)
  for(date.i in seq_along(all.date.vec)){
    
    past.rain.vec <- met.df$PPT_mm_30min[met.df$Date<= all.date.vec[date.i] &
                                           met.df$Date >= (all.date.vec[date.i] - 30)]
    # if(length(past.rain.vec)<30*48){
    #   met.df$past.30.rain.mm[met.df$Date == all.date.vec[date.i]] <- NA
    # }else{
    met.df$past.30.rain.mm[met.df$Date == all.date.vec[date.i]] <- sum(past.rain.vec) 
    # }
  }
  
  asm.ls[[i]] <- met.df
  
}

asm.df <- do.call(rbind,asm.ls)
out.nm <- 'cache/AU-ASM.flux.met.rds'
saveRDS(asm.df,out.nm)

# 
# ####
fn.vec <- list.files(path = 'cache/',pattern = 'flux.met.rds',full.names = T)
hiD.period.ls <- list()
for(i in seq_along(fn.vec)){
  hiD.period.ls[[i]] <- readRDS(fn.vec[[i]])
}

# }
saveRDS(hiD.period.ls,'cache/processed.flx.met.data.rds')

