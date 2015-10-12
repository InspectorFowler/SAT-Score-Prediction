repeater<-function(data,list.out=FALSE){
  
  # NOTE : Works on either High School Dataset or High School Dataset.full
  # and outputs an updated version of the same file with a separate column
  # for Repeat cases.
  
  # Finding student Id's along with grade and year of repetition
  list<-data.frame(id=numeric(),Grade=numeric(),Year=numeric())
  x<-1
  for (i in 2:nrow(data)){
    if(data$Student.ID[i]==data$Student.ID[i-1]){
      if(data$Grade.Level[i]==data$Grade.Level[i-1]){
        if(data$cal.endYear[i]!=data$cal.endYear[i-1]){
          list[x,1]<-data$Student.ID[i]
          list[x,2]<-data$Grade.Level[i]
          list[x,3]<-data$cal.endYear[i]
          x<-x+1
        }
      }
    }
  }
  
  # Flagging the specific student cases with a separate column names Repeat
  data$Repeat<-0
  for (i in 1:nrow(data)){
    if(sum(data$Student.ID[i]==list[,1])>0){
      index<-which(data$Student.ID[i]==list[,1])
      for (j in 1:length(index)){
        if(list[index[j],2]==data$Grade.Level[i]&&list[index[j],3]==data$cal.endYear[i]){
          data$Repeat[i]<-1
        }
      }
    }
  }
  
  #conditionally outputing the whole dataset or the repeat cases only.
  if(list.out==FALSE) return(data)
  else return(list)
}