library(tidyverse)
library(maps)
library(RColorBrewer)
library(viridis)
library(raster)

wr<-map_data("world")
pred<-read.csv("resource.csv")
crop<-subset(pred, Predominant_land_use=="Cropland")

chordata<-subset(crop, Phylum=="Chordata")
chordata$Best_guess_binomial[chordata$Best_guess_binomial==""] <- NA
chordata<-chordata[!is.na(chordata$Best_guess_binomial),]

insecta<-subset(crop, Class=="Insecta")
insecta$Best_guess_binomial[insecta$Best_guess_binomial==""] <- NA
insecta$Best_guess_binomial[insecta$Order==""] <- NA
insecta<-insecta[!is.na(insecta$Best_guess_binomial),]
insecta<-insecta[!is.na(insecta$Order),]

#~adding yield
maize<-read.csv("maize_yield_2010.csv")
soybean<-read.csv("soybean_yield_2010.csv")
rice<-read.csv("rice_yield_2010.csv")
wheat<-read.csv("wheat_yield_2010.csv")

qbin<-function(df){
  names(df)=c("lon", "lat", "yield")
  qq<-quantile(df$yield)
  names(qq)=NULL
  for (i in 1:nrow(df)){
    if (df$yield[i]<qq[2]){
      df$rank[i] <- paste("A. Below 1st quartile", sep="")
      df$cat[i]<-1
    } else if (df$yield[i] >=qq[2] & df$yield[i]<qq[3]){
      df$rank[i] <- paste("B. Between 1st and 2nd quartile", sep="")
      df$cat[i]<-2
    } else if (df$yield[i] >=qq[3]& df$yield[i]<qq[4]){
      df$rank[i] <- paste("C. Between 2nd and 3rd quartile", sep="")
      df$cat[i]<-3
    } else if (df$yield[i] >=qq[4]){
      df$rank[i] <- paste("D. Above 3rd quartile", sep="")
      df$cat[i]<-4
      
    } else {
      df$rank[i]<-NA
      df$cat[i]<-NA
    }
  }
  df<-df[!is.na(df$rank),]
  return(df)
}

maize<-qbin(maize)
rice<-qbin(rice)
soybean<-qbin(soybean)
wheat<-qbin(wheat)

colz<-brewer.pal(4, "OrRd")

#Plotting functions and operations
yield_only<-function(cropdf, crop_name){
  png(paste("yield_2010_", crop_name, ".png", sep=""), width=12, height=8, units="in", res=100)
  a<-ggplot(wr, aes(x = long, y = lat, group=group))+
    geom_polygon(fill = NA, colour = "black", size=0.2)+
    geom_raster(data =cropdf, aes(x = lon, y = lat, fill=rank), inherit.aes = FALSE)+
    scale_y_continuous(limits=c(-60,90))+
    scale_size_continuous(range=c(0.1,20), guide=FALSE)+
    scale_fill_manual(values=colz)+
    
    guides(fill=guide_legend(title="Yield categories")) +
    labs(title = paste("2010 ", crop_name, " yield (Iizumi, 2017)", sep="")) +
    theme(legend.position="bottom")+
    theme_void()+ coord_equal()
  
  print(a)
  dev.off()  
}
#Add predicts data
plotit<-function(cropdf, crop_name, predicts, pred_group, Taxa){
  png(paste("croplands_predicts_", crop_name, "_", pred_group, ".png", sep=""), width=12, height=8, units="in", res=100)
  a<-ggplot(wr, aes(x = long, y = lat, group=group))+
    geom_polygon(fill = NA, colour = "black", size=0.2)+
    geom_raster(data =cropdf, aes(x = lon, y = lat, fill=rank), inherit.aes = FALSE)+
    geom_point(data=predicts, aes(x = Longitude, y = Latitude, color=Taxa, size=Effort_corrected_measurement),
               inherit.aes = FALSE, alpha=0.2)+
    
    
    scale_y_continuous(limits=c(-60,90))+
    scale_size_continuous(range=c(0.1,20), guide=FALSE)+
    scale_color_viridis(discrete=TRUE)+
    scale_fill_manual(values=colz)+
    
    guides(fill=guide_legend(title="Yield categories")) +
    labs(title = paste("2010 ", crop_name, " yield (Iizumi, 2017) and PREDICTS ", pred_group, " cropland species abundance", sep="")) +
    theme(legend.position="bottom")+
    theme_void()+ coord_equal()
  
  print(a)
  dev.off()  
}

##Plot yield maps only
yield_only(maize, "Maize")
yield_only(rice, "Rice")
yield_only(wheat, "Wheat")
yield_only(soybean, "Soybean")

plotit(maize, "Maize", chordata, "vertebrates", chordata$Class)
plotit(rice, "Rice",chordata, "vertebrates", chordata$Class)
plotit(wheat, "Wheat",chordata, "vertebrates", chordata$Class)
plotit(soybean, "Soybean",chordata, "vertebrates", chordata$Class)

plotit(maize, "Maize", insecta, "insects", insecta$Order)
plotit(rice, "Rice",insecta, "insects", insecta$Order)
plotit(wheat, "Wheat",insecta, "insects", insecta$Order)
plotit(soybean, "Soybean",insecta, "insects", insecta$Order)