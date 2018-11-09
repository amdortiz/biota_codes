library(tidyverse)
library(maps)
library(RColorBrewer)
library(viridis)
library(raster)

process_estat<-function(tiffile, vname, crop_name){
  df<-data.frame(rasterToPoints(raster(paste(tiffile, sep=""))))
  names(df)=c("lon", "lat", "variable")
  df<-subset(df, variable!=0)
  qq<-quantile(df$variable)
  names(qq)=NULL
  for (i in 1:nrow(df)){
    if (df$variable[i]<qq[2]){
      df$rank[i] <- paste("A. Below 1st quartile", sep="")
      df$cat[i]<-1
    } else if (df$variable[i] >=qq[2] & df$variable[i]<qq[3]){
      df$rank[i] <- paste("B. Between 1st and 2nd quartile", sep="")
      df$cat[i]<-2
    } else if (df$variable[i] >=qq[3] & df$variable[i]<qq[4]){
      df$rank[i] <- paste("C. Between 2nd and 3rd quartile", sep="")
      df$cat[i]<-3
    } else if (df$variable[i] >=qq[4]){
      df$rank[i] <- paste("D. Above 3rd quartile", sep="")
      df$cat[i]<-4

    } else {
      df$rank[i]<-NA
      df$cat[i]<-NA
    }
  }
  df<-df[!is.na(df$rank),]
  names(df)=c("lon", "lat", vname, "rank", "cat")
  write.csv(df, paste(crop_name, "_", vname, ".csv", sep=""), row.names=FALSE)
  return(df)
}

qbin_csv<-function(dfname, vname, crop_name){
  df<-read.csv(paste(dfname, sep=""))
  names(df)=c("lon", "lat", "variable")
  df<-subset(df, variable!=0)
  qq<-quantile(df$yield)
  names(qq)=NULL
  for (i in 1:nrow(df)){
    if (df$variable[i]<qq[2]){
      df$rank[i] <- paste("A. Below 1st quartile", sep="")
      df$cat[i]<-1
    } else if (df$variable[i] >=qq[2] & df$variable[i]<qq[3]){
      df$rank[i] <- paste("B. Between 1st and 2nd quartile", sep="")
      df$cat[i]<-2
    } else if (df$variable[i] >=qq[3] & df$variable[i]<qq[4]){
      df$rank[i] <- paste("C. Between 2nd and 3rd quartile", sep="")
      df$cat[i]<-3
    } else if (df$variable[i] >=qq[4]){
      df$rank[i] <- paste("D. Above 3rd quartile", sep="")
      df$cat[i]<-4

    } else {
      df$rank[i]<-NA
      df$cat[i]<-NA
    }
  }
  df<-df[!is.na(df$rank),]
  names(df)=c("lon", "lat", vname, "rank", "cat")
  write.csv(df, paste(crop_name, "_", vname, ".csv", sep=""), row.names=FALSE)
  return(df)
}
# colz<-brewer.pal(4, "OrRd")
# wr<-map_data("world")
#
# plot_estat<-function(cropdf, vname, crop_name){
#   png(paste(vname, "_", crop_name, ".png", sep=""), width=12, height=8, units="in", res=100)
#   a<-ggplot(wr, aes(x = long, y = lat, group=group))+
#     geom_polygon(fill = NA, colour = "black", size=0.2)+
#     geom_raster(data =cropdf, aes(x = lon, y = lat, fill=rank), inherit.aes = FALSE)+
#     scale_y_continuous(limits=c(-60,90))+
#     scale_size_continuous(range=c(0.1,20), guide=FALSE)+
#     scale_fill_manual(values=colz)+
#     guides(fill=guide_legend(title=paste(crop_name, " ", vname, sep="")) +
#     theme(legend.position="bottom")+
#     theme_void()+ coord_equal()
#
#   return(a)
#   dev.off()
# }
