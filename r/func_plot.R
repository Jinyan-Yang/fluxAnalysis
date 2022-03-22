library(quantreg)
# C
t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}

hiD.period.ls <- readRDS('cache/processed.flx.met.data.rds')
#############
out.ls <- list()
pdf('tranFrac.pdf',width = 10,height = 10)
for (i in seq_along(hiD.period.ls)){

  # subset data ; filter out bad data
  tmp.df <- hiD.period.ls[[i]]
  tmp.df <- tmp.df[tmp.df$Qle.qc==0,]
  tmp.df <- tmp.df[tmp.df$Rnet> 
                     quantile(tmp.df$Rnet,probs = 0.5,na.rm=T)[[1]] &
                     tmp.df$vpd_kPa>2,]
  tmp.df$e.frac <- tmp.df$Qle / tmp.df$Rnet
  tmp.df <- tmp.df[tmp.df$e.frac>=0 & tmp.df$e.frac<=1,]
  tmp.df$sens.h.frac <- 1-tmp.df$e.frac
  # 
  ref.qle <- quantile(tmp.df$Qle,probs = 0.95,na.rm=T)
  tmp.df$reduction.qle <- 1-tmp.df$Qle / ref.qle[[1]]

  # 
  tmp.df$rained <- NA
  tmp.df$rained[tmp.df$past.30.rain.mm>10] <- 1
  tmp.df$rained[tmp.df$past.30.rain.mm<=10] <- 0
  tmp.df$rained[is.na(tmp.df$past.30.rain.mm)] <- 'Missing'
  tmp.df$rained <- as.factor(tmp.df$rained)
  
  if(nrow(tmp.df[tmp.df$rained == 0,])!=0 &
     nrow(tmp.df[tmp.df$rained == 1,])!=0 ){
    # fit quatile regression
    quantile.fit.dry <- rq(e.frac~vpd_kPa,
                           data = tmp.df[tmp.df$rained == 0,],tau = 0.9)
    quantile.fit.wet <- rq(e.frac~vpd_kPa,
                           data = tmp.df[tmp.df$rained == 1,],tau = 0.9)
    # # try linear fit doesn't think this is a better approach
    # quantile.fit.dry <- lm(reduction.qle~vpd_kPa,data = tmp.df[tmp.df$rained ==0,])
    # quantile.fit.wet <- lm(reduction.qle~vpd_kPa,data = tmp.df[tmp.df$rained ==1,])
    
    # prepare output for fitting
    pft.nm <- gsub(' ','',substr(unique(tmp.df$veg_type),1,9))
    # 
    out.ls[[i]] <- list(site = unique(tmp.df$Site)[!is.na(unique(tmp.df$Site))],
                        pft = pft.nm[!is.na(pft.nm)],
                        tree.grass = NA,
                        fitted.val.dry = quantile.fit.dry$fitted.values,
                        vpd.in.dry=quantile.fit.dry$x[,2],
                        slope.dry=coef(quantile.fit.dry)[[2]],
                        intercpt.dry=coef(quantile.fit.dry)[[1]],
                        reduction.80.dry=(0.8 - coef(quantile.fit.dry)[[1]]) / coef(quantile.fit.dry)[[2]],
                        fitted.val.wet=quantile.fit.wet$fitted.values,
                        vpd.in.wet=quantile.fit.wet$x[,2],
                        slope.wet=coef(quantile.fit.wet)[[2]],
                        intercpt.wet=coef(quantile.fit.wet)[[1]],
                        reduction.80.wet=(0.8 - coef(quantile.fit.wet)[[2]]) / coef(quantile.fit.wet)[[1]])
    
    # out.ls[[i]]$fitted.val.dry <- quantile.fit.dry$fitted.values
    # out.ls[[i]]$vpd.in.dry <- quantile.fit.dry$x[,2]
    # out.ls[[i]]$slope.dry <- coef(quantile.fit.dry)[[2]]
    # out.ls[[i]]$intercpt.dry <- coef(quantile.fit.dry)[[1]]
    # out.ls[[i]]$reduction.80.dry <- (0.8 - out.ls[[i]]$intercpt.dry) / out.ls[[i]]$slope.dry
    # 
    # out.ls[[i]]$fitted.val.wet <- quantile.fit.wet$fitted.values
    # out.ls[[i]]$vpd.in.wet <- quantile.fit.wet$x[,2]
    # out.ls[[i]]$slope.wet <- coef(quantile.fit.wet)[[2]]
    # out.ls[[i]]$intercpt.wet <- coef(quantile.fit.wet)[[1]]
    # out.ls[[i]]$reduction.80.wet <- (0.8 - out.ls[[i]]$intercpt.wet) / out.ls[[i]]$slope.wet
    # 
    is.forest <- grep(pattern = 'for',unique(tmp.df$veg_type),ignore.case=T)
    is.grass <- grep(pattern = 'gra',unique(tmp.df$veg_type),ignore.case=T)
    if(length(is.forest)>0){
      out.ls[[i]]$tree.grass = 'forest'
    }else if(length(is.grass)>0){
      out.ls[[i]]$tree.grass = 'forest'
    }else{
      out.ls[[i]]$tree.grass = 'other'
    }
    
    # quantile.fit.wet <- nlrq(sens.h.frac ~ SSlogis(vpd_kPa, Asym, mid, scal), 
    #                          data = tmp.df[tmp.df$past.30.rain.mm>10,],tau=0.8)
    # 
    # quantile.fit.dry <- nlrq(reduction.qle ~ SSlogis(vpd_kPa, Asym, mid, scal),
    #                          data = tmp.df[tmp.df$rained==0,],
    #                          tau=0.8)
    # 
    # quantile.fit.dry <- nlrq(reduction.qle ~ SSlogis(vpd_kPa, Asym, mid, scal),
    #                          data = tmp.df[tmp.df$rained==1,],
    #                          tau=0.2)
    # 
    # quantile.fit.dry <- nls(sens.h.frac ~ SSlogis(vpd_kPa, Asym, mid, scal), algorithm = "plinear",
    #                          data = tmp.df[tmp.df$past.30.rain.mm<=10,] )
    
    
    # make plot
    par(mfrow=c(2,2))
    palette(c(t_col('coral'),t_col('darkseagreen'),'grey'))
    plot(e.frac~vpd_kPa,data = tmp.df,ylim=c(0,1),pch=16,col=rained,cex=0.7,
         xlab='VPD (kPa)',ylab='ET fraction')
    ylab='Reduction in latent heat flux'
    # vpd.range <- seq(2,7,by=0.1)
    # 
    # lines(vpd.range, (1-predict(quantile.fit.wet, newdata=list(vpd_kPa=vpd.range))), col=3)
    # lines(vpd.range, (1-predict(quantile.fit.dry, newdata=list(vpd_kPa=vpd.range))), col=3)
    abline(quantile.fit.dry, col = "red", lty = 2,lwd = 4)
    abline(quantile.fit.wet, col = "navy", lty = 2,lwd = 4)
    # legend('topright',title = paste0(unique(tmp.df$Site),'_',
    #                                   gsub(' ','',substr(unique(tmp.df$veg_type),1,9))),
    #        legend = c('Wet','Dry','Unknown'),pch=16,col=palette(),
    #        bty='n')
    
    legend('bottomright',title = paste0(out.ls[[i]]$site,'_', out.ls[[i]]$pft),
           legend = c('Wet','Dry','Unknown'),pch=16,col=palette(),horiz = T)
    # 
    plot(Qle~vpd_kPa,data = tmp.df,cex=0.5,xlab='VPD (kPa)',ylab=expression('Latent heat flux'~(W~m^-2)),
         pch=16,col=rained)
    abline(rq(Qle~vpd_kPa,data = tmp.df,tau = 0.9), col = "black", lty = 2)
    
    # 
    hist(tmp.df$LAI[tmp.df$rained == 0],main = '',freq = F,col='coral',xlab='LAI',border = NA)
    hist(tmp.df$LAI[tmp.df$rained == 1],freq = F,add=T,col = t_col('navy'),border = NA)
    legend('topleft',legend = c('Dry','Wet'),
           pch=15,col=c('coral','navy'),bty='n')
    # 
    hist(tmp.df$Qle[tmp.df$rained == 0],main = '',freq = F,col='coral',xlab='LE',border = NA)
    (hist(tmp.df$Qle[tmp.df$rained == 1],freq = F,add=T,col = t_col('navy'),border = NA))
  }
  
 
}
saveRDS(out.ls,'cache/fit.out.rds')

dev.off()
# x <- tmp.df$Rnet
# y <- with(tmp.df,c(Qle/Rnet))
# z <- kde2d(x, y, n = 50)
# contour(z, lwd = 2,
#         col = hcl.colors(15, "Spectral"),ylim=c(0,1))

plot(Qle~vpd_kPa,data = tmp.df)
plot(Qh~vpd_kPa,data = hiD.period.ls[[1]])
plot(c(Qle/Rnet)~vpd_kPa,data = tmp.df,ylim=c(0,1))

names(nc.met$var)
names(nc.flux$var)
nc.flux$var$Qle$units
nc.flux$var$Rnet$units
