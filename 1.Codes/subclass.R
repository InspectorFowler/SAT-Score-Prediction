subclass<-function(data,list){
  
  #The function identifies the subject class
  
  # NOTE : Works on either High School Dataset or High School Dataset.full
  # and outputs an updated version of the same file with a separate column
  # for subject subclass. Needs the Subject list High School as well.
  
  # WARNING : The code is suboptimal and would take around 20-30 mins to run

  #Ensuring that the data formats for both the list and data are same for correct matching
  data$Course.Name<-as.character(data$Course.Name)
  list[,1]<-as.character(list[,1])
  list[,2]<-as.character(list[,2])
  
  new<-numeric(nrow(data))
  
  for (i in 1:nrow(data)){
    for (j in 1:nrow(list)){
      if (list[j,1]==data$Course.Name[i]){
        new[i]=list[j,2]
      }
    }
  }
  data<-cbind(data,as.data.frame(new))
  names(data)[ncol(data)]="subclass"
  
  # Writing file to system
  write.csv(new,"C:/Users/ParikshitVerma/Downloads/Project/2.Excel Data/temp.csv",row.names=FALSE)
}