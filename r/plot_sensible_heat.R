#read fitting####
out.ls <- readRDS('cache/fit.out.rds')
# get fit info
fits.ls <- lapply(out.ls, function(ls.in){
  vpd.vec <- ls.in$model.plc$pred$x
  
  data.out <- data.frame(site = ls.in$site,
                         pft =ls.in$pft,
                         tree_grass = ls.in$tree.grass,
                         vpd.obs.max = max(vpd.vec,na.rm=T),
                         lat = ls.in$lat[!is.na(ls.in$lat)],
                         lon = ls.in$lon[!is.na(ls.in$lon)],
                         # vpd.dry = ls.in[[4]],
                         # vpd.wet=ls.in[[5]],
                         vpd.day.median = sd(vpd.vec,na.rm=T),
                         vpd.freq = length(vpd.vec[vpd.vec>=5]) / length(vpd.vec),
                         slope.fit = coef(ls.in$model.plc)[1,1],
                         slope.fit.025 = coef(ls.in$model.plc)[1,2],
                         slope.fit.975 = coef(ls.in$model.plc)[1,3],
                         
                         p80.fit = coef(ls.in$model.plc)[2,1],
                         p80.fit.025 = coef(ls.in$model.plc)[2,2],
                         p80.fit.975 = coef(ls.in$model.plc)[2,3]
  )
})

site.info.df <- do.call(rbind,fits.ls)
site.info.df$slope.significant <- 1
site.info.df$slope.significant[site.info.df$slope.fit <0 | 
                                 site.info.df$slope.fit.025<0 | 
                                 site.info.df$slope.fit.975<0] <- 0
#get pft### $
# 
site.info.df <- site.info.df[site.info.df$pft != 'Permanent',]
forest.index <- which(site.info.df$pft %in% c("MixedFor","Evergreen","Deciduous"))
sav.index <- which(site.info.df$pft %in% c('Savannas','Savannas:','WoodySav'))
grass.index <- which(site.info.df$pft %in% c("Grassland"))
shrub.index <- which(site.info.df$pft %in% c('OpenShru','ClosedSh:','ClosedSh'))
crop.index <- which(site.info.df$pft %in% c('Croplands'))

length(forest.index)
length(sav.index)
length(grass.index)
length(shrub.index)
length(crop.index)
# get name
site.info.df$pft.name <- NA
site.info.df$pft.name[forest.index] <- 'forests'
site.info.df$pft.name[sav.index] <- 'savanna'
site.info.df$pft.name[grass.index] <- 'grasslands'
site.info.df$pft.name[shrub.index] <- 'shublands'
site.info.df$pft.name[crop.index] <- 'croplands'
site.info.df$pft.name <- factor(site.info.df$pft.name,levels = c('croplands','forests','savanna','grasslands','shublands'))
# get pft number
site.info.df$pft.num <- NA
set.seed(1935)
site.info.df$pft.num[crop.index] <- 1+rnorm(length(crop.index),mean = 0,sd=0.1)
site.info.df$pft.num[forest.index] <- 2+rnorm(length(forest.index),mean = 0,sd=0.1)
site.info.df$pft.num[sav.index] <- 3+rnorm(length(sav.index),mean = 0,sd=0.1)
site.info.df$pft.num[grass.index] <- 4+rnorm(length(grass.index),mean = 0,sd=0.1)
site.info.df$pft.num[shrub.index] <- 5+rnorm(length(shrub.index),mean = 0,sd=0.1)
site.info.df <- site.info.df[!is.na(site.info.df$pft.num),]

# get if fitted vpd80 is within observation range
site.info.df$in.range <- NA
site.info.df$in.range[site.info.df$p80.fit< site.info.df$vpd.obs.max] <- 2
site.info.df$in.range[site.info.df$p80.fit>= site.info.df$vpd.obs.max] <- 1
# 
sig.df <- site.info.df[site.info.df$slope.significant == 1 & 
                         site.info.df$in.range == 2 &
                         site.info.df$p80.fit > 2 & 
                         site.info.df$p80.fit < 10,]

# get tmax
hiD.period.ls <- readRDS('cache/processed.flx.met.data.rds')

palette(col.df$iris[c(2,4,5,1,3)])
# ###########
for (i in 1:nrow(sig.df)) {
  ls.num <- as.numeric(rownames(sig.df)[[i]])
  
  tmp.df <- hiD.period.ls[[ls.num]]
  
  tmp.df$below.thrshold <- NA
  tmp.df$below.thrshold[tmp.df$vpd_kPa< sig.df$p80.fit[i]] <- 'Low VPD'
  tmp.df$below.thrshold[tmp.df$vpd_kPa >= sig.df$p80.fit[i]] <- 'High VPD'
  tmp.df$below.thrshold <- as.factor(tmp.df$below.thrshold)
  tmp.df$vpd.80 <-  sig.df$p80.fit[[i]]
  
  if(i==1){
    
    # plot(Qh~vpd_kPa,data = tmp.df[tmp.df$below.thrshold == 'High VPD',],xlim=c(5,10),
    #      pch=16,col = sig.df$pft.name[i])
    # plot(Rnet~lat,data = tmp.df[tmp.df$below.thrshold == 'High VPD',],xlim=c(-60,60),pch=16,col = sig.df$pft.name[i],ylim=c(0,800))
    plot(vpd.80~lat,data = tmp.df[tmp.df$below.thrshold == 'High VPD',],xlim=c(-60,60),pch=16,col = sig.df$pft.name[i],ylim=c(4,10),
         xlab='Latitude',ylab=expression(VPD[80]~(kPa)))
  }else{
    # points(Qh~vpd_kPa,data = tmp.df[tmp.df$below.thrshold == 'High VPD',],
    #      pch=16,col = sig.df$pft.name[i])
    # points(Rnet~lat,data = tmp.df[tmp.df$below.thrshold == 'High VPD',],pch=16,col = sig.df$pft.name[i])
    
    points(vpd.80~lat,data = tmp.df[tmp.df$below.thrshold == 'High VPD',],xlim=c(-60,60),pch=16,col = sig.df$pft.name[i],ylim=c(4,10),
         xlab='Latitude',ylab=expression(VPD[80]~(kPa)))
  }
}
vpd.tmp.ls <- list()
for (i in 1:nrow(sig.df)) {
  ls.num <- as.numeric(rownames(sig.df)[[i]])
  
  tmp.df <- hiD.period.ls[[ls.num]]
  
  tmp.df$below.thrshold <- NA
  tmp.df$below.thrshold[tmp.df$vpd_kPa< sig.df$p80.fit[i]] <- 'Low VPD'
  tmp.df$below.thrshold[tmp.df$vpd_kPa >= sig.df$p80.fit[i]] <- 'High VPD'
  tmp.df$below.thrshold <- as.factor(tmp.df$below.thrshold)
  
  tmp.df$veg_type <- sig.df$pft.name[[i]]
  
  vpd.tmp.ls[[i]] <- tmp.df

}
vpd.tmp.df <- do.call(rbind,vpd.tmp.ls)
vpd.tmp.df <- vpd.tmp.df[vpd.tmp.df$Rnet>100,]
# hist(vpd.tmp.df$Rnet[vpd.tmp.df$veg_type == 'forests'& vpd.tmp.df$below.thrshold == 'High VPD'])


# rnet
for (i in seq_along(unique(vpd.tmp.df$veg_type))) {
  tmp.sub.df <- vpd.tmp.df[vpd.tmp.df$veg_type == unique(vpd.tmp.df$veg_type)[i] & 
                             vpd.tmp.df$below.thrshold == 'High VPD',]
  
  data = tmp.sub.df$Rnet
  dens=density(data[!is.na(data)])
  # dens.future =density(vpd.future[!is.na(vpd.future)])
  
  if(i==1){
    
    plot(dens$x,dens$y,type="l",xlab="Rnet",ylim=c(0,0.0025),
         ylab="Density",lwd=2,col=t_col(i,percent = 0))
  }else{
    points(dens$x,dens$y,type="l",
         ylab="Density",lwd=2,col=t_col(i,percent = 0))
  }
}
legend('topright',legend = levels(sig.df$pft.name),lty='solid',col=palette(),bty='n')
# sensible heat ###########
png('figures/sensible heat distribution.png',width = 500,height = 2*500*.618)
par(mar=c(5,5,1,1),
    mfrow=c(2,1))
levelspft <- levels(droplevels(vpd.tmp.df$veg_type))
palette(col.df$iris[c(2,4,5,1,3)])
for (i in seq_along(levelspft)) {
  tmp.sub.df <- vpd.tmp.df[vpd.tmp.df$veg_type == levelspft[i] & 
                             vpd.tmp.df$below.thrshold == 'High VPD',]
  
  data = tmp.sub.df$Qh
  dens=density(data[!is.na(data)])
  # dens.future =density(vpd.future[!is.na(vpd.future)])
  
  if(i==1){
    
    plot(dens$x,dens$y,type="l",xlab=expression("Sensible Heat "*(W~m^-2)),ylim=c(0,0.007),xlim=c(-50,700),
         ylab="Density",lwd=3,col=t_col(i,percent = 0))
  }else{
    points(dens$x,dens$y,type="l",
           ylab="Density",lwd=3,col=t_col(i,percent = 0))
  }
}
legend('topright',legend = levelspft,lty='solid',col=palette(),bty='n',lwd=3)
legend('topleft',legend = '(a)',bty='n')
# netR
for (i in seq_along(levelspft)) {
  tmp.sub.df <- vpd.tmp.df[vpd.tmp.df$veg_type == levelspft[i] & 
                             vpd.tmp.df$below.thrshold == 'High VPD',]
  
  data = tmp.sub.df$Rnet
  dens=density(data[!is.na(data)])
  # dens.future =density(vpd.future[!is.na(vpd.future)])
  
  if(i==1){
    
    plot(dens$x,dens$y,type="l",xlab=expression("Net radiation "*(W~m^-2)),ylim=c(0,0.012),
         ylab="Density",lwd=2,col=t_col(i,percent = 0))
  }else{
    points(dens$x,dens$y,type="l",
           ylab="Density",lwd=2,col=t_col(i,percent = 0))
  }
}
# legend('topright',legend = levels(sig.df$pft.name),lty='solid',col=palette(),bty='n')
legend('topleft',legend = '(b)',bty='n')
dev.off()
# plot by lat
for (i in 1:nrow(sig.df)) {
  ls.num <- as.numeric(rownames(sig.df)[[i]])
  
  tmp.df <- hiD.period.ls[[ls.num]]
  
  tmp.df$below.thrshold <- NA
  tmp.df$below.thrshold[tmp.df$vpd_kPa< sig.df$p80.fit[i]] <- 'Low VPD'
  tmp.df$below.thrshold[tmp.df$vpd_kPa >= sig.df$p80.fit[i]] <- 'High VPD'
  tmp.df$below.thrshold <- as.factor(tmp.df$below.thrshold)
  tmp.df$vpd.80 <-  sig.df$p80.fit[[i]]
  
  if(i==1){
    
    # plot(Qh~vpd_kPa,data = tmp.df[tmp.df$below.thrshold == 'High VPD',],xlim=c(5,10),
    #      pch=16,col = sig.df$pft.name[i])
    # plot(Rnet~lat,data = tmp.df[tmp.df$below.thrshold == 'High VPD',],xlim=c(-60,60),pch=16,col = sig.df$pft.name[i],ylim=c(0,800))
    plot(vpd.80~lat,data = tmp.df[tmp.df$below.thrshold == 'High VPD',],xlim=c(-40,60),pch=16,col = sig.df$pft.name[i],ylim=c(3,10),
         xlab='Latitude',ylab=expression(VPD[80]~(kPa)))
  }else{
    # points(Qh~vpd_kPa,data = tmp.df[tmp.df$below.thrshold == 'High VPD',],
    #      pch=16,col = sig.df$pft.name[i])
    # points(Rnet~lat,data = tmp.df[tmp.df$below.thrshold == 'High VPD',],pch=16,col = sig.df$pft.name[i])
    
    points(vpd.80~lat,data = tmp.df[tmp.df$below.thrshold == 'High VPD',],xlim=c(-60,60),pch=16,col = sig.df$pft.name[i],ylim=c(4,10),
           xlab='Latitude',ylab=expression(VPD[80]~(kPa)))
  }
}


# # 
# 
# plot(dens$x,dens$y,type="l",xlab="VPD (kPa)",
#        ylab="Density",lwd=2,col=t_col(1,percent = 0),ylim=c(0,0.003))
# 
#   
# points(dens.future$x,dens.future$y,type="l",lwd=2,col=t_col(col.in,percent = 0))
#   
#   
#   
# 

# plot(Rnet~below.thrshold,data = tmp.df[tmp.df$Rnet>300,])
# 
# plot(Qh~below.thrshold,data = tmp.df[tmp.df$Rnet>300,])


