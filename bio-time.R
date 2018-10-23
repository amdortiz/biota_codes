##Read files
bio <-read.csv("BioTIMEQuery02_04_2018.csv")
tm<-read.csv("bt.csv") #terrestrial studies by number only (based on metadata)
tm<-tm$study

terra<-subset(bio, STUDY_ID %in% tm)
marine<-subset(bio, !(STUDY_ID %in% tm))

##Sample study extraction function
study_extract<-function(habitat,study_num,study.lname){
  study.name<-subset(habitat, STUDY_ID==study_num)
  newlist<-list()
  study.lname<-assign(paste(study.lname, sep=""), newlist)
  num_species<-unique(study.name$GENUS_SPECIES)
  
  maxy<-vector()
  miny<-vector()
  for (i in 1:length(num_species)){
    study.lname[[i]]<-subset(study.name, GENUS_SPECIES==num_species[i])
    study.lname[[i]]=study.lname[[i]][,c("LATITUDE", "LONGITUDE", "YEAR", "sum.allrawdata.ABUNDANCE")]
    
    maxy[i]<-max(study.lname[[i]]$YEAR)
    miny[i]<-min(study.lname[[i]]$YEAR)
    
    temp<-list()
    for (j in miny[i]:maxy[i]){
      temp[[j]]<-subset(study.lname[[i]], YEAR==j)
      zz<-temp[[j]][,1:3]
      zz<-apply(zz,2,mean)
      names(zz)=NULL
      temp[[j]]<-apply(temp[[j]], 2, sum)
      temp[[j]]<-c(zz, temp[[j]][4])
    }
    rownames(study.lname[[i]])=NULL
    study.lname[[i]]<-temp
    study.lname[[i]]<-data.frame(do.call(rbind.data.frame, study.lname[[i]]), num_species[i])
    names(study.lname[[i]])=c("LATITUDE", "LONGITUDE", "YEAR", "ABUNDANCE", "SPECIES")
  }
  return(study.lname)
}

##Example fetching code; study numbers are in metadata
study_475_list<-study_extract(terra,475,"study_475_list")

##Sample plot
plot(ABUNDANCE ~ YEAR, data=study_475_list[[31]], type='o', main='Turdus merula (blackbird) abundance, BioTIME')