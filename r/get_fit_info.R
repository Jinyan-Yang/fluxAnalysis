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
sig.df <- site.info.df[site.info.df$slope.significant == 1 & site.info.df$p80.fit > 2,]

# get tmax
hiD.period.ls <- readRDS('cache/processed.flx.met.data.rds')
t.ls <- list()
for (i in seq_along(hiD.period.ls)){
  tmp.df <- hiD.period.ls[[i]]
  t.ls[[i]] <- data.frame(site = unique(tmp.df$Site)[!is.na(unique(tmp.df$Site))],
                          # pft = pft.nm[!is.na(pft.nm)],
                          lat = unique(tmp.df$lat),
                          lon = unique(tmp.df$lon),
                          tmax= max(tmp.df$Tair,na.rm=T) -272.15)
}
t.df <- do.call(rbind,t.ls)
t.df$tmax[t.df$tmax<0] <- t.df$tmax[t.df$tmax<0] + 272.15

sig.df <- merge(sig.df,t.df,all.x=T)

saveRDS(sig.df,'cache/fit.slople.sig.rds')

nrow(site.info.df[site.info.df$slope.significant == 1 &
                    site.info.df$p80.fit > 2,]) / nrow(site.info.df)

nrow(site.info.df[site.info.df$slope.significant == 1 &
                    site.info.df$p80.fit > 2 &
                    site.info.df$in.range==2,]) / nrow(site.info.df)

nrow(sig.df[sig.df$p80.fit <10,])
