source('r/functions_plot.R')
source('r/get_fit_info.R')
# sig.df <- readRDS('cache/fit.slople.sig.rds')
high.d.df <- sig.df[sig.df$p80.fit <10,]
# make plots####
tiff('figures/LE decline fit.tif',width = 400*2,height = 400*3*.618)

par(mar=c(5,5,1,1),mfrow=c(3,2),cex=1.1)

plot.fit.plc.func(forest.index = crop.index,out.ls = out.ls,col.plot = col.df$iris[2],sig.vec = site.info.df$slope.significant)
legend('topleft',legend = c('(a) croplands'),bty='n')

plot.fit.plc.func(forest.index = forest.index,out.ls = out.ls,col.plot = col.df$iris[4],sig.vec = site.info.df$slope.significant)
legend('topleft',legend = c('(b) forests'),bty='n')

plot.fit.plc.func(forest.index = sav.index,out.ls = out.ls,col.plot = col.df$iris[5],sig.vec = site.info.df$slope.significant)
legend('topleft',legend = c('(c) savanna'),bty='n')

plot.fit.plc.func(forest.index = grass.index,out.ls = out.ls,col.plot = col.df$iris[1],sig.vec = site.info.df$slope.significant)
legend('topleft',legend = c('(d) grasslands'),bty='n')

plot.fit.plc.func(forest.index = shrub.index,out.ls = out.ls,col.plot = col.df$iris[3],sig.vec = site.info.df$slope.significant)
legend('topleft',legend = c('(e) shublands'),bty='n')
# 
# plobarplot()
palette(col.df$iris[c(2,4,5,1,3)])
pch.vec <- c(1,16)
plot((p80.fit)~pft.num,data = sig.df,
     ylim=(c(2,10)),xaxt='n',pch=pch.vec[in.range],col = round(pft.num),
     ylab = expression(VPD[80]~(kPa)),xlab=' ',yaxt='n')
# add ci
arrows(x0 = sig.df$pft.num, y0 = sig.df$p80.fit.025,
       x1 =sig.df$pft.num, y1=sig.df$p80.fit.975,
       length=0.01, angle=90, code=3,col=round(sig.df$pft.num),lty=c('dotted','solid')[sig.df$in.range])


axis(side = 1,at = 1:5,labels = levels(sig.df$pft.name),las=2)
# axis(side = 2,at = log(c(2,seq(5,30,by=5))),labels = c(2,seq(5,30,by=5)))
axis(side = 2,at = seq(2,10,by=2),labels = seq(2,10,by=2))
legend('topleft',legend = c('(f)'),bty='n')
dev.off()

# plot threshold vs frequency ####
tiff('figures/vpd80.tif',width = 500,height = 500*3*.618)
par(mar=c(5,5,1,1),mfrow=c(3,1),cex=1)
high.d.df <- sig.df[sig.df$p80.fit <10,]
# a
pch.vec <- c(1,16)
plot(p80.fit~vpd.obs.max,data = high.d.df,col=round(pft.num),pch=16,#pch.vec[in.range],
     ylim=c(2,10),
     xlab=expression(VPD[max]~(kPa)),ylab=expression(VPD[80]~(kPa),xlim=c(3,10)))
arrows(x0 = high.d.df$vpd.obs.max, y0 = high.d.df$p80.fit.025,
       x1 = high.d.df$vpd.obs.max, y1=high.d.df$p80.fit.975,
       length=0.01, angle=90, code=3,col=round(high.d.df$pft.num),lty='solid')
legend('topleft',legend = '(a)',bty='n')
legend('bottomleft',legend = levels(high.d.df$pft.name),col=palette(),bty='n',horiz = F,pch=16,ncol=3)
# b
# df.sub <- high.d.df[high.d.df$pft.name=='grasslands' |
#                       high.d.df$pft.name == 'forests',]

df.sub <- high.d.df[high.d.df$pft.name != 'savanna' &
                      high.d.df$pft.name!= 'croplands',]
plot(p80.fit~vpd.obs.max,data = df.sub,
     col=round(pft.num),pch=16,#pch.vec[in.range],
     ylim=c(2,10),
     xlab=expression(VPD[max]~(kPa)),ylab=expression(VPD[80]~(kPa)),cex=2)

lm.fit <- lm(p80.fit~vpd.obs.max ,data = df.sub)

abline(lm.fit, col='grey',lwd=3,lty='dashed' )
legend('topleft',legend = '(b)',bty='n')

mylabel = bquote(italic(R)^2 == .(format(summary(lm.fit)$r.squared, digits = 3)))
text(x = 6.5, y = 8, labels = mylabel)
# c
df.sub <- high.d.df[high.d.df$pft.name == 'savanna' | 
                      high.d.df$pft.name== 'croplands',]
plot(p80.fit~vpd.obs.max,data = df.sub,
     col=round(pft.num),pch=16,#pch.vec[in.range],
     ylim=c(2,10),
     xlab=expression(VPD[max]~(kPa)),ylab=expression(VPD[80]~(kPa)),cex=2)

lm.fit <- lm(p80.fit~vpd.obs.max ,data = df.sub)

abline(lm.fit, col='grey',lwd=3,lty='dashed' )
legend('topleft',legend = '(c)',bty='n')

mylabel = bquote(italic(R)^2 == .(format(summary(lm.fit)$r.squared, digits = 3)))
text(x = 8.5, y = 6, labels = mylabel)

dev.off()


nrow(sig.df[sig.df$p80.fit <10,]) / nrow(sig.df)
nrow(sig.df[sig.df$p80.fit <10,])/71


# plot vpd80 and tmax###################
palette(col.df$iris[c(2,4,5,1,3)])
tiff('figures/vpd80_tmax.tif',width = 500,height = 500*3*.618)
par(mar=c(5,5,1,1),mfrow=c(3,1),cex=1)

# a
pch.vec <- c(1,16)
plot(p80.fit~tmax,data = high.d.df,col=round(pft.num),pch=16,#pch.vec[in.range],
     ylim=c(2,10),
     xlab=expression(T[max]~(degree*C)),ylab=expression(VPD[80]~(kPa),xlim=c(32,52)))
arrows(x0 = high.d.df$vpd.obs.max, y0 = high.d.df$p80.fit.025,
       x1 = high.d.df$vpd.obs.max, y1=high.d.df$p80.fit.975,
       length=0.01, angle=90, code=3,col=round(high.d.df$pft.num),lty='solid')
legend('topleft',legend = '(a)',bty='n')
legend('bottomleft',legend = levels(high.d.df$pft.name),col=palette(),bty='n',horiz = F,pch=16,ncol=3)

# 
df.sub <- high.d.df[high.d.df$pft.name == 'forests' |
                            high.d.df$pft.name== 'grasslands',]
# b
plot(p80.fit~tmax,data = df.sub,
     col=round(pft.num),pch=16,#pch.vec[in.range],
     ylim=c(2,10),xlim=c(32,52),
     xlab=expression(T[max]~(degree*C)),ylab=expression(VPD[80]~(kPa)),cex=2)

lm.fit <- lm(p80.fit~tmax ,data = df.sub)

abline(lm.fit, col='grey',lwd=3,lty='dashed' )
legend('topleft',legend = '(b)',bty='n')

mylabel = bquote(italic(R)^2 == .(format(summary(lm.fit)$r.squared, digits = 3)))
text(x = 45, y = 8, labels = mylabel)
# legend('topright',legend = c(mylabel,
#                              paste0('n forest = ',nrow(df.sub[df.sub$pft.name=='forests',])),
#                              paste0('n grasslands = ',nrow(df.sub[df.sub$pft.name=='grasslands',]))
#                              ),
#        bty='n')
# c
df.sub <- high.d.df[high.d.df$pft.name == 'savanna' | 
                            high.d.df$pft.name== 'croplands',]
plot(p80.fit~tmax,data = df.sub,
     col=round(pft.num),pch=16,#pch.vec[in.range],
     ylim=c(2,10),xlim=c(32,52),
     xlab=expression(T[max]~(degree*C)),ylab=expression(VPD[80]~(kPa)),cex=2)

lm.fit <- lm(p80.fit~tmax ,data = df.sub)

abline(lm.fit, col='grey',lwd=3,lty='dashed' )
legend('topleft',legend = '(c)',bty='n')

mylabel = bquote(italic(R)^2 == .(format(summary(lm.fit)$r.squared, digits = 3)))
text(x = 48, y = 6, labels = mylabel)

dev.off()

# abline(lm(p80.fit~vpd.obs.max,data =high.d.df),col='grey',lwd=2)

# summary(lm(p80.fit~vpd.obs.max ,data = high.d.df))
# summary(lm(p80.fit~vpd.day.median ,data = sig.df[sig.df$vpd.freq>0,]))
# summary(lm(p80.fit~abs(lat) ,data = high.d.df))

# # forest
# f.df <- high.d.df[high.d.df$pft.name=='forests' & high.d.df$p80.fit<10,]
# 
# plot(p80.fit~vpd.obs.max,data = f.df,
#      col=round(pft.num),pch=16,ylim=c(2,10),xlim=c(3,10),
#      xlab=' Max daytime VPD (kPa)',ylab=expression(VPD[80]~(kPa)))
# abline(lm(p80.fit~vpd.obs.max,data =f.df),col='grey',lwd=2)
# summary(lm(p80.fit~vpd.obs.max ,data = f.df))$r.squared
# 
# 
# 
# points(p80.fit~vpd.obs.max,data = high.d.df[high.d.df$pft.name=='grasslands',],
#      col=round(pft.num),pch=16,ylim=c(2,10),xlim=c(3,10))
# abline(lm(p80.fit~vpd.obs.max,data =high.d.df[high.d.df$pft.name=='grasslands',]),
#        col='grey',lwd=2,lty='dashed' )
# summary(lm(p80.fit~vpd.obs.max ,data = high.d.df[high.d.df$pft.name=='grasslands',]))$r.squared





# plot(p80.fit~vpd.day.median,data = high.d.df,col=round(pft.num),pch=16,ylim=c(2,10))
# plot(p80.fit~abs(lat),data = high.d.df,col=round(pft.num),pch=16,ylim=c(2,10))



# # 
# summary(lm(p80.fit~pft.num,data = sig.df[sig.df$pft.name %in% c('forests','grasslands'),]))
# median(sig.df$p80.fit[sig.df$pft.name == 'forests'& sig.df$in.range==2])
# median(sig.df$p80.fit[sig.df$pft.name == 'grasslands' & sig.df$in.range==2])
# shrub.df <- site.info.df[shrub.index,]
# 
# nrow(sig.df[sig.df$pft.name %in% c('forests','grasslands'),]) / nrow(sig.df)


# sig.df$pft.name <- as.character(sig.df$pft.name)
# ano.table <- aov(p80.fit~pft.name,data = sig.df)
# summary(ano.table)

# 
# 
# fits.ls <- lapply(out.ls, function(ls.in){
#   data.out <- data.frame(site = ls.in[[1]],
#                          pft =ls.in[[2]],
#                          tree_grass = ls.in[[3]],
#                          vpd.dry = ls.in[[4]],
#                          vpd.wet=ls.in[[5]],
#                          vpd.day.median = max(ls.in[[6]]$pred$x),
#                          slope.fit = coef(ls.in[[6]])[2,1],
#                          slope.fit.025 = coef(ls.in[[6]])[2,2],
#                          slope.fit.975 = coef(ls.in[[6]])[2,3],
#                          
#                          p80.fit = coef(ls.in[[6]])[1,1],
#                          p80.fit.025 = coef(ls.in[[6]])[1,2],
#                          p80.fit.975 = coef(ls.in[[6]])[1,3]
#                          )
# })
# fit.site.df <- do.call(rbind,fits.ls)
# fit.site.df$slope.fit.correct <- fit.site.df$slope.fit
# 
# fit.site.df$slope.fit.correct[fit.site.df$slope.fit.correct < 0 | 
#                                 fit.site.df$slope.fit.025<0 | 
#                                 fit.site.df$slope.fit.975<0] <- 0
# 
# fit.site.df$tree_grass <- as.factor(fit.site.df$tree_grass)
# 
# palette(c('forestgreen','coral'))
# plot((slope.fit.correct)~vpd.day.median,data = fit.site.df,pch=16,col=tree_grass)
# 
# plot((p80.fit)~vpd.day.median,data = fit.site.df,pch=16,col=tree_grass)
