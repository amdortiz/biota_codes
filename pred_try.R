library(tidyverse)
library(maps)
library(RColorBrewer)
# library(wesanderson)
library(viridis)
library(raster)


wr<-map_data("world")

pred<-read.csv("resource.csv")
crop<-subset(pred, Predominant_land_use=="Cropland")
crop$Best_guess_binomial[crop$Best_guess_binomial==""] <- "NA"
crop<-crop[!is.na(crop$Best_guess_binomial),]

# lc_type<-c(
#   "Evergreen Needleleaf forest", #1
#   "Evergreen Broadleaf forest", #2
#   "Deciduous Needleleaf forest", #3
#   "Deciduous Broadleaf forest", #4
#   "Mixed forest", #5
#   # "Closed shrublands",
#   # "Open shrublands",
#   # "Woody savannas",
#   # "Savannas",
#   # "Grasslands",
#   # "Permanent wetlands",
#   "Croplands", #12
#   # "Urban and built-up",
#   "Cropland/Natural vegetation mosaic" #14
#   #, 
#   # "Snow and ice",
#   # "Barren or sparsely vegetated"
# )
lc_type<-"Croplands"


# mypalette<-(wes_palette("Darjeeling1", n=7,type="continuous"))

lc<-data.frame(rasterToPoints(raster("LC_hd_global_2010.tif")))
names(lc)=c("lon", "lat", "cat")
lc<-subset(lc, cat>0) #excluding water
lc<-subset(lc, cat==12)


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
# 
# ## 'GB': ('United Kingdom', (-7.57216793459, 49.959999905, 1.68153079591, 58.6350001085)),






# wrmap<-
#   xx %>% ggplot() +
#   geom_polygon(data = wr, aes(x=long, y = lat, group = group), fill="grey", alpha=0.1) +
#   geom_point(aes(x=Longitude, y=Latitude), alpha=0.5) +
#   scale_size_continuous(range=c(0.2,1)) +
#   coord_equal() +
#   theme_void() +
#   theme(legend.position="none") +
#   xlim(-180,180) +
#   ylim(-60,80) 
# 
# png("plantae.png", width=12, height=8, units="in", res=100)
# par(mar=c(1,1,1,1))
# print(wrmap)
# dev.off()
