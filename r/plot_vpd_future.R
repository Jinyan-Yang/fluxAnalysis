hiD.period.ls <- readRDS('cache/processed.flx.met.data.rds')
source('r/get_fit_info.R')
# https://www.omnicalculator.com/chemistry/vapour-pressure-of-water
sat.vp.func <- function(tair){
  # 610.7 * 10^((7.5*tair)/(237.3+tair)) /1000
  0.61078*exp((17.27*tair)/(tair+237.3))
}
# tmp.df <- hiD.period.ls[[5]]
# # Generate some data
# data=tmp.df$vpd_kPa[tmp.df$Rnet>100]
# # Get the density estimate
# dens=density(data)
# # Plot y-values scaled by number of observations against x values
# plot(dens$x,dens$y,type="l",xlab="VPD (kPa)",ylab="Density")

# plot.vpd.dis.func <- function(hiD.period.ls,forest.index,col.in){
#   for (i in seq_along(forest.index)) {
#     tmp.df <- hiD.period.ls[[forest.index[i]]]
#     tmp.df <- tmp.df[!is.na(tmp.df$Rnet),]
#     tmp.df <- tmp.df[tmp.df$Rnet>0,]
#     tmp.df <- tmp.df[tmp.df$Rnet>quantile(tmp.df$Rnet,probs = 0.6,na.rm=T)[[1]],]
#     
#     t.current <- tmp.df$Tair
#     vp.sat.current <- sat.vp.func((t.current-272.15))
#     vp.sat.4.5 <- sat.vp.func((t.current-272.15+4.5))
#     
#     
#     # Generate some data
#     data=tmp.df$vpd_kPa
#     vpd.future <- data + vp.sat.4.5 - vp.sat.current
#     # Get the density estimate
#     dens=density(data[!is.na(data)])
#     dens.future =density(vpd.future[!is.na(vpd.future)])
#     if(i==1){
#       # Plot y-values scaled by number of observations against x values
#       plot(dens$x,dens$y,type="l",xlab="VPD (kPa)",ylim=c(0,1),xlim=c(0,10),
#            ylab="Density",lwd=2,col=t_col(col.in,percent = 80))
#       points(dens.future$x,dens.future$y,type="l",lwd=2,col=t_col(col.in,percent = 0))
#     }else{
#       points(dens$x,dens$y,type="l",lwd=2,col=t_col(col.in,percent = 80))
#       points(dens.future$x,dens.future$y,type="l",lwd=2,col=t_col(col.in,percent = 0))
#     }
#   }
# }

plot.vpd.dis.func <- function(hiD.period.ls,forest.index,col.in){
  tmp.ls <- list()
  for (i in seq_along(forest.index)) {
    tmp.df <- hiD.period.ls[[forest.index[i]]]
    tmp.df <- tmp.df[!is.na(tmp.df$Rnet),]
    tmp.df <- tmp.df[tmp.df$Rnet>0,]
    tmp.df <- tmp.df[tmp.df$Rnet>quantile(tmp.df$Rnet,probs = 0.6,na.rm=T)[[1]],]
    
    t.current <- tmp.df$Tair
    tmp.df$vp.sat.current <- sat.vp.func((t.current-272.15))
    tmp.df$vp.sat.4.5 <- sat.vp.func((t.current-272.15+4.5))
    
    tmp.ls[[i]] <- tmp.df
    
    
  }
  tmp.all.df <- do.call(rbind,tmp.ls)
  # Generate some data
  data=tmp.all.df$vpd_kPa
  vpd.future <- data + tmp.all.df$vp.sat.4.5 - tmp.all.df$vp.sat.current
  # Get the density estimate
  data[data<0] <- NA
  dens=density(data[!is.na(data)])
  vpd.future[vpd.future=='NaN'] <- NA
  vpd.future[vpd.future<0 | vpd.future>15] <- NA
  dens.future =density(vpd.future[!is.na(vpd.future) & is.finite(vpd.future)])

    # Plot y-values scaled by number of observations against x values
    plot(dens$x,dens$y,type="l",xlab="VPD (kPa)",ylim=c(0,1),xlim=c(0,10),
         ylab="Density",lwd=2,col=t_col(col.in,percent = 80))
    points(dens.future$x,dens.future$y,type="l",lwd=2,col=t_col(col.in,percent = 0))

}

tiff('figures/future vpd.tif',width = 400*2,height = 400*3*.618)
par(mar=c(5,5,1,1),mfrow=c(3,2),cex=1.1)
plot.vpd.dis.func(hiD.period.ls,forest.index = crop.index,col.in =  col.df$iris[2])
legend('topleft',legend = c('(a) croplands'),bty='n')
abline(v = 6.25 ,col='grey')
abline(v = 6.25+0.7301*2   ,col='red')

plot.vpd.dis.func(hiD.period.ls,forest.index = forest.index,col.in =  col.df$iris[4])
legend('topleft',legend = c('(b) forests'),bty='n')
abline(v = 6.25 ,col='grey')
abline(v = 6.25- 0.9894   ,col='red')

plot.vpd.dis.func(hiD.period.ls,forest.index = sav.index,col.in =  col.df$iris[5])
legend('topleft',legend = c('(c) savana'),bty='n')
abline(v = 6.25 ,col='grey')
abline(v = 6.25+0.7301*2   ,col='red')

plot.vpd.dis.func(hiD.period.ls,forest.index = grass.index,col.in =  col.df$iris[1])
legend('topleft',legend = c('(d) grasslands'),bty='n')
abline(v = 6.25 ,col='grey')
abline(v = 6.25- 0.9894   ,col='red')

plot.vpd.dis.func(hiD.period.ls,forest.index = shrub.index,col.in =  col.df$iris[3])
legend('topleft',legend = c('(e) shublands'),bty='n')
abline(v = 6.25 ,col='grey')
abline(v = 6.25- 0.9894   ,col='red')
dev.off()
# for (i in seq_along(forest.index)) {
#   tmp.df <- hiD.period.ls[[forest.index[i]]]
#   tmp.df <- tmp.df[!is.na(tmp.df$Rnet),]
#   tmp.df <- tmp.df[tmp.df$Rnet>400,]
#   
#   t.current <- tmp.df$Tair
#   vp.sat.current <- sat.vp.func((t.current-272.15))
#   vp.sat.4.5 <- sat.vp.func((t.current-272.15+4.5))
#   
#   
#   # Generate some data
#   data=tmp.df$vpd_kPa
#   vpd.future <- data + vp.sat.4.5 - vp.sat.current
#   # Get the density estimate
#   dens=density(data[!is.na(data)])
#   dens.future =density(vpd.future[!is.na(vpd.future)])
#   if(i==1){
#     # Plot y-values scaled by number of observations against x values
#     plot(dens$x,dens$y,type="l",xlab="VPD (kPa)",ylim=c(0,1),
#          ylab="Density",lwd=2,col=t_col(col.df$iris[2],percent = 80))
#     points(dens.future$x,dens.future$y,type="l",lwd=2,col=t_col(col.df$iris[2],percent = 0))
#   }else{
#     points(dens$x,dens$y,type="l",lwd=2,col=t_col(col.df$iris[2],percent = 80))
#     points(dens.future$x,dens.future$y,type="l",lwd=2,col=t_col(col.df$iris[2],percent = 0))
#   }
# }

