fn.vec <- list.files('download/flux_anna/selected/',pattern = 'Met.nc')
fn.vec <- gsub('Met.nc','Flux.nc',fn.vec)

# http://dapds00.nci.org.au/thredds/fileServer/ks32/CLEX_Data/PLUMBER2/v1-0/Flux/AR-SLu_2010-2010_FLUXNET2015_Flux.nc
url.flux <- 'https://dapds00.nci.org.au/thredds/fileServer/ks32/CLEX_Data/PLUMBER2/v1-0/Flux/'

for (i in seq_along(fn.vec)){
  link.i <- paste0(url.flux,fn.vec[i])
  # 
  fn.out <- file.path('download/flux_anna/selected',fn.vec[i])
  # 
  if(file.exists(fn.out)){
    warning(paste0(fn.out,' exists!'))
  }else{
    download.file(url = link.i,
                  destfile = fn.out,method = 'curl',
                  cacheOK = FALSE #this is key because the default true value will not work
    )
    
  }
  
}
