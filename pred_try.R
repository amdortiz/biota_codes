# library(tidyverse)
# library(maps)
# library(RColorBrewer)
# library(viridis)
# library(raster)
# 
# wr<-map_data("world")
# 
# pred<-read.csv("resource.csv")
# crop<-subset(pred, Predominant_land_use=="Cropland")
# crop$Best_guess_binomial[crop$Best_guess_binomial==""] <- "NA"
# crop<-crop[!is.na(crop$Best_guess_binomial),]
# lc_type<-"Croplands"
# 
# lc<-data.frame(rasterToPoints(raster("LC_hd_global_2010.tif")))
# names(lc)=c("lon", "lat", "cat")
# lc<-subset(lc, cat>0) #excluding water
# lc<-subset(lc, cat==12) #croplands
# 
# wrmap<-ggplot(wr, aes(x = long, y = lat, group=group))+
#   geom_polygon(fill = NA, colour = "black", size=0.25)+
#   theme_void()+ coord_equal()+
#   geom_raster(data = lc, aes(x = lon, y = lat, fill=as.factor(cat)),inherit.aes = FALSE)+
#   labs(title = paste("Land cover 2010 (GLCF) and PREDICTS cropland species abundance", sep="")) +
#   scale_fill_manual(values="grey", labels = lc_type)+
#   guides(fill=guide_legend(title="Categories")) +
#   scale_y_continuous(limits=c(-60,90))+
#   geom_point(data=crop, aes(x = Longitude, y = Latitude, color=Class, size=Effort_corrected_measurement),
#                                         inherit.aes = FALSE, alpha=0.2)+theme(legend.position="bottom")+
#   scale_size_continuous(range=c(0.1,20), guide=FALSE)+
#   scale_color_viridis(discrete=TRUE)

#~adding yield

maize<-read.csv("maize_yield_2010.csv")
soybean<-read.csv("soybean_yield_2010.csv")
rice<-read.csv("rice_yield_2010.csv")
wheat<-read.csv("wheat_yield_2010.csv")

#manual bins: maize/rice
mr_bin<-function(df){
  if (df$yield<0){
    df$rank<-NA
  } else if (df$yield <11){
    df$rank <- "Low (0-10 t/ha)"
  } else if (df$yield >10 & df$yield<20){
    df$rank <- "Mid (11-20 t/ha)"
  } else if (df$yield >19){
    df$rank <- "High (Above 20 t/ha)"
  } else {
    df$rank<-NA
  }
  return(df)
}

#manual bins: soy/wheat
sw_bin<-function(df){
  if (df$yield<0){
    df$rank<-NA
  } else if (df$yield <4){
    df$rank <- "Low (0-3 t/ha)"
  } else if (df$yield >3 & df$yield<8){
    df$rank <- "Mid (4-7 t/ha)"
  } else if (df$yield >7){
    df$rank <- "High (Above 8 t/ha)"
  } else {
    df$rank<-NA
  }
  return(df)
}

+ geom_raster(data = df[[i]], aes(x = lon, y = lat, fill=yield),inherit.aes = FALSE)+
  scale_fill_gradientn(colours=mypalette)


# png("croplands_predicts.png", width=12, height=8, units="in", res=100)
# par(mar=c(1,1,1,1))
# print(wrmap)
# dev.off()