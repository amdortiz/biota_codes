rm(list=ls())
library(raster)
library(tidyverse)

#reading in data from PREDICTS and land use
pred<-read.csv("PREDICTS_NatPlusCrop_forestBiome_site_level.csv")
pred<-data.frame(pred$Longitude, pred$Latitude)
names(pred)=c("lon", "lat")
kehoe<-raster("kehoe_df.tif")

min_dist<-vector()
for (i in 1:nrow(pred)){
  xmin<-pred[i,]$lon -0.5 #degrees
  xmax<-pred[i,]$lon +0.5
  ymin<-pred[i,]$lat -0.5
  ymax<-pred[i,]$lat +0.5
  test<-crop(kehoe, c(xmin,xmax,ymin,ymax))
  #single point
  pred_test=pred[i,]
  
  #subset to the cropped area
  test_pts <- rasterToPoints(test, function(x){!is.na(x)})
  
  #testing if grid does not have any forest at all
  if (dim(test_pts)[1]<=1){
    xmin<-pred[i,]$lon -1
    xmax<-pred[i,]$lon +1
    ymin<-pred[i,]$lat -1
    ymax<-pred[i,]$lat +1
    test<-crop(kehoe, c(xmin,xmax,ymin,ymax))
    #single point
    pred_test=pred[i,]
    #subset to the cropped area
    test_pts <- rasterToPoints(test, function(x){!is.na(x)})
  }
  if (dim(test_pts)[1]<=1){
    xmin<-pred[i,]$lon -10
    xmax<-pred[i,]$lon +10
    ymin<-pred[i,]$lat -10
    ymax<-pred[i,]$lat +10
    test<-crop(kehoe, c(xmin,xmax,ymin,ymax))
    #single point
    pred_test=pred[i,]
    #subset to the cropped area
    test_pts <- rasterToPoints(test, function(x){!is.na(x)})
  }
  #actual minimum distance calc
  min_dist[i]<-min(pointDistance(pred_test, test_pts[,1:2], lonlat = TRUE, allpairs = TRUE)/1000) #closest distance
  test_pts<-NULL
}

predog<-read.csv("PREDICTS_NatPlusCrop_forestBiome_site_level.csv")
predog<-data.frame(predog, min_dist)
write.csv(predog, "min_dist_pred_site_level.csv", row.names = FALSE)
