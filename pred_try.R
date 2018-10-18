library(tidyverse)
library(maps)
library(RColorBrewer)
library(viridis)
library(raster)

wr<-map_data("world")

pred<-read.csv("resource.csv")
crop<-subset(pred, Predominant_land_use=="Cropland")
crop$Best_guess_binomial[crop$Best_guess_binomial==""] <- "NA"
crop<-crop[!is.na(crop$Best_guess_binomial),]
lc_type<-"Croplands"

lc<-data.frame(rasterToPoints(raster("LC_hd_global_2010.tif")))
names(lc)=c("lon", "lat", "cat")
lc<-subset(lc, cat>0) #excluding water
lc<-subset(lc, cat==12) #croplands

wrmap<-ggplot(wr, aes(x = long, y = lat, group=group))+
  geom_polygon(fill = NA, colour = "black", size=0.25)+
  theme_void()+ coord_equal()+
  geom_raster(data = lc, aes(x = lon, y = lat, fill=as.factor(cat)),inherit.aes = FALSE)+
  labs(title = paste("Land cover 2010 (GLCF) and PREDICTS cropland species abundance", sep="")) +
  scale_fill_manual(values="grey", labels = lc_type)+
  guides(fill=guide_legend(title="Categories")) +
  scale_y_continuous(limits=c(-60,90))+
  geom_point(data=crop, aes(x = Longitude, y = Latitude, color=Class, size=Effort_corrected_measurement),
                                        inherit.aes = FALSE, alpha=0.2)+theme(legend.position="bottom")+
  scale_size_continuous(range=c(0.1,20), guide=FALSE)+
  scale_color_viridis(discrete=TRUE)

# scale_color_manual(values=a)+
png("croppredicts.png", width=12, height=8, units="in", res=100)
par(mar=c(1,1,1,1))
print(wrmap)
dev.off()