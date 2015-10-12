indicator<-function(data){
  
  # NOTE : The code inputs the combined Enrollment and Repeat dataset
  # and ouputs the same dataset with an additional column indicating repeat
  # year datapoints
  
  data$Date<-as.Date(as.character(data$Date),format="%m/%d/%Y")
  data$Indicator<-1
  for (i in 1:nrow(data)){
    col.no<-which(names(data)==paste("X",data$Grade[i],sep=""))
    end.date<-as.Date(paste("7/1",data[i,col.no],sep="/"),format="%m/%d/%Y")
    if(data$Date[i]<=end.date) data$Indicator[i]<-0
  }
  
  # Creating the concatenated Grade-Repeat Column
  data$Grade<-paste(paste("X",data$Grade,sep=""),data$Indicator,sep=".")
  data<-data[,-c(2,3,4,5,ncol(data))]
  
  # Writing file to system
  write.csv(data,"C:/Users/ParikshitVerma/Downloads/Project/2.Excel Data/temp.csv",row.names=FALSE)
}

