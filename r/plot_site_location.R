source('r/get_fit_info.R')
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