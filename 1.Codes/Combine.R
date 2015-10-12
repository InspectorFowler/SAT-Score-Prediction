combine<-function(directory,skip=0){
  
  # The following function reads the directory and calculates the total number 
  # of high, middle and elementary schools and stacks them on a school level basis
  
  dir<-list.files(directory)
  fyear<-15
  
  # Total number of schools in the AF Network
  high<-length(grep("1.",dir))
  middle<-length(grep("2.",dir))
  elementary<-length(grep("3.",dir))
  
  #Combining all the yearly files into one dataset for all schools
  for (i in (1+skip):length(dir)){
    subdir<-list.files(paste(directory,dir[i],sep="/"))
    cdata<-data.frame()
    for (j in 1:length(subdir)){
      index<-paste(sprintf("%02d",fyear-j),"-",sprintf("%02d",fyear-j+1),".csv",sep="")
      data<-read.csv(paste(paste(directory,dir[i],sep="/"),index,sep="/"))
      cdata<-rbind(cdata,data)
    }
    write.csv(cdata,paste(directory,dir[i],"combined.csv",sep="/"),row.names=FALSE)
  }
}