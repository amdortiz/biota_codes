library(raster)
library(rgeos)
library(ClusterR)
tree<-raster("Hansen.tif")
# tree[tree<81]<-NA
# writeRaster(tree, filename="site_trial_1.tif", format="GTiff", overwrite=TRUE)

m <- c(0, 80, NA, 80,100,1)
m <- matrix(m, ncol=3, byrow=TRUE)

beginCluster(n = 6,type='SOCK')

rc1 <- clusterR(tree, reclassify, args=list(rcl=m, right=FALSE),
                filename='site_trial_2.tif', datatype='INT2S', overwrite=TRUE)

endCluster()