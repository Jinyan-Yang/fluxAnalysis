source('r/functions_plot.R')
# ############
hiD.period.ls <- readRDS('cache/processed.flx.met.data.rds')
#############
out.ls <- list()
pdf('le.plcfit.pdf',width = 8,height = 8*2*.618)
for (i in seq_along(hiD.period.ls)){
  
  # subset data ; filter out bad data
  tmp.df <- hiD.period.ls[[i]]
  tmp.df <- tmp.df[tmp.df$Qle.qc==0,]
  tmp.df <- tmp.df[tmp.df$Rnet> 
                     quantile(tmp.df$Rnet,probs = 0.6,na.rm=T)[[1]] &
                     tmp.df$vpd_kPa>2,]
  tmp.df$e.frac <- tmp.df$Qle / tmp.df$Rnet
  tmp.df <- tmp.df[tmp.df$e.frac>=0 & tmp.df$e.frac<=1,]
  tmp.df$sens.h.frac <- 1-tmp.df$e.frac
  # 
  ref.qle <- quantile(tmp.df$Qle[tmp.df$vpd_kPa<2.5],probs = 0.95,na.rm=T)
  tmp.df$qle.realised.frac <- tmp.df$Qle / ref.qle[[1]]
  tmp.df$reduction.qle <- 1-tmp.df$qle.realised.frac
  
  # 
  tmp.df$rained <- NA
  tmp.df$rained[tmp.df$past.30.rain.mm>10] <- 1
  tmp.df$rained[tmp.df$past.30.rain.mm<=10] <- 0
  tmp.df$rained[is.na(tmp.df$past.30.rain.mm)] <- 'Missing'
  tmp.df$rained <- as.factor(tmp.df$rained)
  
  
  tmp.df$plc <- (1-tmp.df$qle.realised.frac)*100
  tmp.df <- tmp.df[tmp.df$plc>1,]
  # tmp.df$Weights <- 1/(1+exp(tmp.df$vpd_kPa-2))#100*(tmp.df$vpd_kPa-2)
  
  nl.fit <- fitplc(tmp.df[tmp.df$rained!=0 & !is.na(tmp.df$plc),],varnames = c(PLC = "plc", WP = "vpd_kPa"),model = "sigmoidal",x=80)
  # plot(nl.fit)
  # prepare output for fitting
  pft.nm <- gsub(' ','',substr(unique(tmp.df$veg_type),1,9))
  
  tmp.out.ls <- list(site = unique(tmp.df$Site)[!is.na(unique(tmp.df$Site))],
                     pft = pft.nm[!is.na(pft.nm)],
                     lat = unique(tmp.df$lat),
                     lon = unique(tmp.df$lon),
                     tree.grass = NA,
                     vpd.in.dry=max(tmp.df$vpd_kPa[tmp.df$rained == 0],na.rm=T),
                     vpd.in.wet=max(tmp.df$vpd_kPa[tmp.df$rained == 1],na.rm=T),
                     model.plc = nl.fit)
  out.ls[[i]] <- tmp.out.ls
  
  # make plot
  par(mfrow=c(2,1))
  palette(c(t_col('navy'),'grey'))
  
  plot(nl.fit,xlab='VPD (kPa)',ylab='Relative LE',linecol ='red',linelwd =4,pointcol ='grey',cicol = t_col("red", 70))
  legend('topright',title = paste0(out.ls[[i]]$site,'_', out.ls[[i]]$pft),
         legend = c('Wet','Unknown'),pch=16,col=palette(),horiz = T,bty='n')
  
  legend('topleft',legend = sprintf('(%s)',i),bty='n')
  
  plot(Qle~vpd_kPa,data = tmp.df[tmp.df$rained !=0 & !is.na(tmp.df$plc),],
       cex=0.5,xlab='VPD (kPa)',ylab=expression('Latent heat flux'~(W~m^-2)),
       pch=16,col=rained)
  abline(rq(Qle~vpd_kPa,data = tmp.df,tau = 0.9), col = "black", lty = 2)
  
}
saveRDS(out.ls,'cache/fit.out.rds')

dev.off()
# test <- out.ls[[1]]
