library(raster)
library(rgeos)
tree<-raster("Hansen.tif")
tree[tree<81]<-NA
writeRaster(tree, filename="site_trial_1.tif", format="GTiff", overwrite=TRUE)
