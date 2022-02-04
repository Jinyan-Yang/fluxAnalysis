# https://dap.nci.org.au/thredds/remoteCatalogService?catalog=http://dapds00.nci.org.au/thredds/catalog/ks32/CLEX_Data/PLUMBER2/v1-0/Met/catalog.xml

library(ncdf4)
siteInfo.df <- readLines('siteInfo.txt')

fn.vec <- siteInfo.df[grep('_Met.nc',siteInfo.df)]

for (i in seq_along(fn.vec)
     ) {
  tmp.nm <- substring(fn.vec[i], 1,regexpr("Met.nc", fn.vec[i])+5 )
  tmp.nm <- gsub('<dataset name=\"','',tmp.nm)

  tmp.url <- paste0('https://dapds00.nci.org.au/thredds/fileServer/ks32/CLEX_Data/PLUMBER2/v1-0/Met/',
                    tmp.nm)
  # 
  file.out <- file.path('download','flux_anna',tmp.nm)
  if(file.exists(file.out)){
    warning(paste0(file.out,' exists!'))
  }else{
    download.file(url = tmp.url,
                  destfile = file.out,method = 'curl',
                  cacheOK = FALSE #this is key because the default true value will not work
                  )

  }

}












# 
# 'https://dapds00.nci.org.au/thredds/fileServer/ks32/CLEX_Data/PLUMBER2/v1-0/Met/AT-Neu_2002-2012_FLUXNET2015_Met.nc'
# x <- 'http://dapds00.nci.org.au/thredds/fileServer/ks32/CLEX_Data/PLUMBER2/v1-0/Flux/AR-SLu_2010-2010_FLUXNET2015_Flux.nc'
# download.file(x,'test.nc')
# getURL(x)
# getURL(x, ssl.verifypeer = FALSE)
# 
# 
# nc.tmp <- nc_open('test.nc')
# nc.tmp <- nc_open(file.out)
# nc_close(nc.tmp)
# nc.tmp <- nc_open('download/flux_anna/AT-Neu_2002-2012_FLUXNET2015_Met.nc')
