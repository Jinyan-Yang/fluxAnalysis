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
sat.vp.func <- function(tair){
  610.7 * 10^((7.5*tair)/(237.3+tair)) /1000
}
sat.vp.func(40)

hiD.period.ls <- readRDS('cache/processed.flx.met.data.rds')





site.5.df <- hiD.period.ls[[8]]
site.5.df$tair.celcius <- site.5.df$Tair -272.15
site.5.df$vp.sat <- sat.vp.func(site.5.df$tair.celcius)
site.5.df$vp.sat.2plus <- sat.vp.func(site.5.df$tair.celcius+2)
site.5.df$vp.sat.4.5plus <- sat.vp.func(site.5.df$tair.celcius+4.5)
site.5.df$vpd.2plus <- site.5.df$vp.sat.2plus + site.5.df$vpd_kPa - site.5.df$vp.sat
site.5.df$vpd.4.5plus <- site.5.df$vp.sat.4.5plus + site.5.df$vpd_kPa - site.5.df$vp.sat

hist(site.5.df$vpd_kPa[site.5.df$Rnet>300])
hist(site.5.df$vpd.2plus[site.5.df$Rnet>300],add=TRUE,col=t_col('red',percent = 80))
hist(site.5.df$vpd.4.5plus[site.5.df$Rnet>300],add=TRUE,col=t_col('purple',percent = 80))

median(site.5.df$vp.sat - site.5.df$vpd_kPa)

plot(vpd_kPa~tair.celcius,data = site.5.df)

points(x = site.5.df$tair.celcius,
       y = sat.vp.func(site.5.df$tair.celcius) - 1.6,
       type='l',col='red',lwd=3)
