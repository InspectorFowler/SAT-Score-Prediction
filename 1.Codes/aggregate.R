aggregate<-function(data){
  
  # NOTE: WOrks on the Sparse High School Gradebook and 
  # outputs Consolidated High School Subclass Grades
  
  # Creating an Empty data frame to store the aggregated grade scores
  out.data<-data.frame(Student.ID=numeric(),
                       Grade.Level=numeric(),
                       Repeat=numeric(),
                       Subclass=character(),
                       Score=numeric(),
                       stringsAsFactors=FALSE)
  
  # Initializing the values of counters and temporary variables
  out.count<-1
  if (is.na(data$Final.Score[1])==FALSE){
    x<-data$Final.Score[1]
    count<-1
  }
  else{
    x<-0
    count<-0
  }
  
  # Loop over the entire dataset to aggregate the data
  for (i in 2:nrow(data)){
    if (data[i,1]==data[i-1,1]){
      if (data[i,2]==data[i-1,2]){
        if (data[i,3]==data[i-1,3]){
          if(data[i,4]==data[i-1,4]){
            if(is.na(data$Final.Score[i])==FALSE){
              x<-x+data$Final.Score[i]
              count<-count+1
            }
          }
          else{
            out.data[out.count,1]<-data[i-1,1]
            out.data[out.count,2]<-data[i-1,2]
            out.data[out.count,3]<-data[i-1,3]
            out.data[out.count,4]<-as.character(data[i-1,4])
            if(is.nan(x/count)==FALSE) out.data[out.count,5]<-round(x/count,1)
            out.count<-out.count+1
            if (is.na(data$Final.Score[i])==FALSE){
              x<-data$Final.Score[i]
              count<-1
            }
            else{
              x<-0
              count<-0
            }
          }
        }
        else{
          out.data[out.count,1]<-data[i-1,1]
          out.data[out.count,2]<-data[i-1,2]
          out.data[out.count,3]<-data[i-1,3]
          out.data[out.count,4]<-as.character(data[i-1,4])
          if(is.nan(x/count)==FALSE) out.data[out.count,5]<-round(x/count,1)
          out.count<-out.count+1
          if (is.na(data$Final.Score[i])==FALSE){
            x<-data$Final.Score[i]
            count<-1
          }
          else{
            x<-0
            count<-0
          }
        }
      }
      else{
        out.data[out.count,1]<-data[i-1,1]
        out.data[out.count,2]<-data[i-1,2]
        out.data[out.count,3]<-data[i-1,3]
        out.data[out.count,4]<-as.character(data[i-1,4])
        if(is.nan(x/count)==FALSE) out.data[out.count,5]<-round(x/count,1)
        out.count<-out.count+1
        if (is.na(data$Final.Score[i])==FALSE){
          x<-data$Final.Score[i]
          count<-1
        }
        else{
          x<-0
          count<-0
        }
      }
    }
    else{
      out.data[out.count,1]<-data[i-1,1]
      out.data[out.count,2]<-data[i-1,2]
      out.data[out.count,3]<-data[i-1,3]
      out.data[out.count,4]<-as.character(data[i-1,4])
      if(is.nan(x/count)==FALSE) out.data[out.count,5]<-round(x/count,1)
      out.count<-out.count+1
      if (is.na(data$Final.Score[i])==FALSE){
        x<-data$Final.Score[i]
        count<-1
      }
      else{
        x<-0
        count<-0
      }
    }
  }
  
  #Creating flags for Arts, Sports and Other Subjects instead of whole scores
  
  for (i in 1:nrow(out.data)){
    if ((out.data$Subclass[i]=="Arts"|out.data$Subclass[i]=="Sports"|
         out.data$Subclass[i]=="Other")&is.na(out.data$Score[i])==FALSE) out.data$Score[i]=1
  }
 
  # Writing file to system
  write.csv(out.data,"C:/Users/ParikshitVerma/Downloads/Project/2.Excel Data/temp.csv",row.names=FALSE)
}