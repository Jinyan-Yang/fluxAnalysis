
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")
library(fitplc)
library(metafor)
# 
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

# plot function###########
plot.fit.plc.func <- function(forest.index,out.ls,col.plot,sig.vec){
  
  for (plot.i in seq_along(forest.index)){
    tmp.ls <- out.ls[[forest.index[plot.i]]]
    # col.plot <- 'darkseagreen'
    
    if(sig.vec[forest.index[plot.i]]==1){
      lty.sig = 'solid'
    }else{
      lty.sig='dotted'
    }
    
    if(plot.i ==1){
      plot(tmp.ls$model.plc,xlim=c(2,10),
           linecol =col.plot,linelwd =2,pch=NA,linetype = lty.sig,
           pointcol ='grey',cicol = t_col(col.plot, 80),pxlinecol = NA,ylim=c(0,0.8),citype = "polygon",px_ci_label = F,pxcex = 0.001,
           xlab='VPD (kPa)',ylab='Relative LE (0-1)',plotPx =F)
    }else{
      plot(tmp.ls$model.plc,xlim=c(2,10),
           linecol =col.plot,linelwd =2,pch=NA,linetype = lty.sig,
           pointcol ='grey',cicol = t_col(col.plot, 80),pxlinecol = NA,px_ci_label = F,citype = "polygon",pxcex = 0.001,
           add =T,plotPx =F)
    }
  }
}
