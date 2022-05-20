source('r/get_fit_info.R')
source('r/functions_plot.R')
# READ tmax from world climate
library(raster)
jan.ra <- raster('download/wc2.1_10m_bio/wc2.1_10m_bio_5.tif',
                 xmn=-180, xmx=180, ymn=-90, ymx=90)
# 
tiff('figures/sites.tif',width = 800,height = 450)
par(mar = c(5,5,5,5))
plot(jan.ra,col=c(rev(cm.colors(6)),rev(heat.colors(9))))

#  add points
points(x = site.info.df$lon,y = site.info.df$lat,pch=1)
points(x = sig.df$lon[sig.df$p80.fit<10],y = sig.df$lat[sig.df$p80.fit<10],pch=16)
dev.off()

# extract world clim tmax
sig.df$tmax.worldClim <- extract(x = jan.ra,y = cbind(sig.df$lon,
                             sig.df$lat))

high.d.df <- sig.df[sig.df$p80.fit <10,]
# #####
tiff('figures/vpd80_tmax_worldClim.tif',width = 500*2,height = 500*2*.618)
par(mar=c(5,5,1,1),mfrow=c(2,2),cex=1)
# a
palette(col.df$iris[c(2,4,5,1,3)])
pch.vec <- c(1,16)
# 
# plot(p80.fit~tmax.worldClim,data = high.d.df,col=round(pft.num),pch=16,#pch.vec[in.range],
#      ylim=c(2,10),
#      xlab=expression(T[max]~(degree*C)),ylab=expression(VPD[80]~(kPa),xlim=c(3,10)))
df.sub <- high.d.df[high.d.df$pft.name== 'forests',]
plot(p80.fit~tmax.worldClim,data = df.sub,
     col=round(pft.num),pch=16,#pch.vec[in.range],
     ylim=c(2,10),
     xlab=expression(T[max]~(degree*C)),ylab=expression(VPD[80]~(kPa)),cex=2,
     xlim=c(24,40))

lm.fit <- lm(p80.fit~tmax.worldClim ,data = df.sub)

abline(lm.fit, col='grey',lwd=3,lty='dashed' )
legend('topleft',legend = '(a)',bty='n')

mylabel = bquote(italic(R)^2 == .(format(summary(lm.fit)$r.squared, digits = 3)))
text(x = 32, y = 8, labels = mylabel)
# b
df.sub <- high.d.df[high.d.df$pft.name == 'savanna' |
                      high.d.df$pft.name== 'grasslands',]
plot(p80.fit~tmax.worldClim,data = df.sub,
     col=round(pft.num),pch=16,#pch.vec[in.range],
     ylim=c(2,10),
     xlab=expression(T[max]~(degree*C)),ylab=expression(VPD[80]~(kPa)),cex=2,
     xlim=c(24,40))

lm.fit <- lm(p80.fit~tmax.worldClim ,data = df.sub)

abline(lm.fit, col='grey',lwd=3,lty='dashed' )
legend('topleft',legend = '(b)',bty='n')

mylabel = bquote(italic(R)^2 == .(format(summary(lm.fit)$r.squared, digits = 3)))
text(x = 36.5, y = 8, labels = mylabel)
# 
# c
df.sub <- high.d.df[
                      high.d.df$pft.name == 'croplands',]
plot(p80.fit~tmax.worldClim,data = df.sub,
     col=round(pft.num),pch=16,#pch.vec[in.range],
     ylim=c(2,10),
     xlab=expression(T[max]~(degree*C)),ylab=expression(VPD[80]~(kPa)),cex=2,
     xlim=c(24,40))

lm.fit <- lm(p80.fit~tmax.worldClim ,data = df.sub)

abline(lm.fit, col='grey',lwd=3,lty='dashed' )
legend('topleft',legend = '(c)',bty='n')

mylabel = bquote(italic(R)^2 == .(format(summary(lm.fit)$r.squared, digits = 3)))
text(x = 28, y = 8, labels = mylabel)
# d
plot(tmax~tmax.worldClim,data = sig.df,col=round(pft.num),pch=16)
lm.fit <- lm(tmax~tmax.worldClim ,data = sig.df)

abline(lm.fit, col='grey',lwd=3,lty='dashed' )
# summary(lm(tmax~tmax.worldClim,data = sig.df))
mylabel = bquote(italic(R)^2 == .(format(summary(lm.fit)$r.squared, digits = 3)))
text(x = 25, y = 45, labels = mylabel)
legend('topleft',legend = '(d)',bty='n')
dev.off()