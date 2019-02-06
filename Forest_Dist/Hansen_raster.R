library(raster)
library(rgeos)
tree<-raster("Hansen.tif")
# tree[tree<81]<-NA
# writeRaster(tree, filename="site_trial_1.tif", format="GTiff", overwrite=TRUE)

f4 <- function(x, a, filename) {
  out <- raster(x)
  bs <- blockSize(out)
  out <- writeStart(out, filename, overwrite=TRUE)
  for (i in 1:bs$n) {
    v <- getValues(x, row=bs$row[i], nrows=bs$nrows[i] )
    v<-ifelse(v >= a, 1, NA)
    out <- writeValues(out, v, bs$row[i])
  }
  out <- writeStop(out)
  return(out)
}
s <- f4(tree, 80, filename='site_trial_1.tif')