\name{process_estat}
\alias{process_estat}
\title{Process earthstat files into csv}
\usage{
process_estat()
}
\description{
Prints 'A way to process the tiff files (raster) into csv with quartle bins. The format is
process_estat<-function(tiffile, vname, crop_name) where tiffile is the name of the file, vname is the name of the variable and crop_name is the name of the crop'.
}
\examples{
maize_yield<-process_estat("maize_yield.tif", "yield", "maize")
}
