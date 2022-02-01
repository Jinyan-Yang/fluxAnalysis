siteInfo.df <- readLines('siteInfo.txt')

fn.vec <- siteInfo.df[grep('_Met.nc',siteInfo.df)]

for (i in 1:4#seq_along(fn.vec)
     ) {
  tmp.nm <- substring(fn.vec[i], 1,regexpr("Met.nc", fn.vec[i])+5 )
  tmp.nm <- gsub('<dataset name=\"','',tmp.nm)
  # fn.vec.cln[i] <- tmp.nm
  # 
                   # http://dapds00.nci.org.au/thredds/fileServer/ks32/CLEX_Data/PLUMBER2/v1-0/Met/AU-ASM_2011-2017_OzFlux_Met.nc
  tmp.url <- paste0('http://dapds00.nci.org.au/thredds/fileServer/ks32/CLEX_Data/PLUMBER2/v1-0/Met/',
                    tmp.nm)
  # 
  file.out <- file.path('download','flux_anna',tmp.nm)
  if(file.exists(file.out)){
    warning(paste0(file.out,' exists!'))
  }else{
    download.file(url = tmp.url,
                  destfile = file.out,quiet = F,method = 'libcurl')
  }

}


nc.tmp <- nc_open(met.fn.vec[i])
