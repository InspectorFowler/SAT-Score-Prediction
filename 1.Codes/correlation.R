correlation<-function(data.sat,data.subjects){

    corr.table<-data.frame(matrix(NA,nrow=(ncol(data.sat)-1),ncol=(ncol(data.subjects))))
    corr.table[,1]<-names(data.sat)[2:ncol(data.sat)]
    names(corr.table)<-names(data.subjects)
    names(corr.table)[1]<-"Subjects"
    for (i in 1:nrow(corr.table)){
      for (j in 2:ncol(corr.table)){
        corr.table[i,j]<-cor(data.sat[,i+1],data.subjects[,j])
      }
    }
    return(corr.table)
}