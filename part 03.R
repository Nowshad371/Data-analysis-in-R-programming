
getwd()
lung = read.csv(file ="C:/Users/Nowshad/Documents/LungCapData.csv")
View(lung)


missing=function(data,variable)
{
  count=sum(is.na(data[variable]))
  print(paste("there is",count,"missing value",names(data[variable])))
}
missing(lung,1)




mat=matrix(1:12,nrow=3,ncol=4)



sum(mat[,1])

ncol(mat)

find = function(data,col)
  {
 
  row = nrow(data)
  sum = 0
  i = 1
  while (i != row + 1) {
    #here i is the name of the dataset and col is number of coloumn
    count = data[i,col]
    if(count > 10){
      
      sum = sum + 1
      
    }
    i = i+1
    
    
  }
  
  print(paste("there is",sum," values which are greater than 10 in ",names(data[col])))
  
  
}


find(lung,1)
























