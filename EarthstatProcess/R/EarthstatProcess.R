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
