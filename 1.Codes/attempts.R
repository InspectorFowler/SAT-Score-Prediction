attempts<-function(data){
  
  #NOTE : Works on the sAT Dataset and outputs an updated version of the
  # the same file with a separate column for number of attempts made.
  
  # Creating an Empty data frame to store the aggregated grade scores
  attempt<-vector(mode="numeric",length=nrow(data))
  attempt[1]<-1
  
  for(i in 2:nrow(data)){
    if (data[i,1]==data[i-1,1]) attempt[i]=attempt[i-1]+1
    else attempt[i]=1
  }
  
  # Bind the vector to the data frame
  out.data<-cbind(data,as.data.frame(attempt))
  names(out.data)[8]<-"Attempt"
  
  # Writing file to system
  write.csv(out.data,"C:/Users/ParikshitVerma/Downloads/Project/2.Excel Data/temp.csv",row.names=FALSE)

}