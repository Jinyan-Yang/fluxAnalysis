# ######
library(dplyr)
library(doBy)
hiD.period.ls <- readRDS('cache/processed.flx.met.data.rds')
flux.vpd.range.ls <- list()
# 
for (site.i in seq_along(hiD.period.ls)) {
  # site.df <- readRDS('cache/AR-SLu.flux.met.rds')
  site.df <- hiD.period.ls[[site.i]]
  site.df$trans.frac <- site.df$Qle / site.df$Rnet
  #perform data binning on points variable
  # site.df.bin <- mutate(site.df[site.df$Qle.qc==0 &
  #                                 site.df$trans.frac>0.18&
  #                                 site.df$trans.frac<0.22,],
  #                       vpd_bin = cut(trans.frac, 
  #                                     breaks=seq(0,1,by=0.5),
  #                                     labels = seq(0.5,10,by=0.5)))
  site.df.bin <- site.df[site.df$Qle.qc==0 &
                           site.df$trans.frac>0.18&
                           site.df$trans.frac<0.22&
                           site.df$Rnet>200,]
  site.df.bin$vpd.90 <- quantile(site.df.bin$vpd_kPa,na.rm=T,probs = 0.9)[[1]]
  flux.vpd.range.ls[[site.i]] <- site.df.bin
}
hist(flux.vpd.range.ls[[1]]$vpd_kPa)


rm(hiD.period.ls)
