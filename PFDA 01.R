

getwd()
lung = read.csv(file ="C:/Users/Nowshad/Documents/LungCapData.csv")

View(lung)


smoke = function(data,variable,col2){
  
  count = nrow(data)
  
  sum = 0
  i = 1
  
  while(i != count + 1){
    
    if(data[i,variable] == "no"&& data[i,col2] >= 17){
      
      sum = sum + 1
    }
    i = i+1
    
    
    
  }
  print(paste("there are ",sum," people smoke ",names(data[variable])))
  
  
}

smoke(lung,4,2)

attach(lung)

class(Smoke)
levels(lung$Smoke)


names(lung)



#it will show number of rows and col
dim(lung)



length(Age)

Age[1:14]



lung[11:14,]

head(lung,10)
tail(lung,5)

mean(Age[Gender=="female"])

femdata <-lung[Gender == "female",]

maledata <-lung[Gender =="male", ]

dim(maledata)

maleOver15 <- lung[Gender == "female" & Age == 15, ]
dim(maleOver15)





temp = Age >15


temp[1:5]

temp = as.numeric(Age>15)
temp[1:5]

lung[1:5, ]

femalesmoke  = Gender == "female" & Smoke == "yes"

femalesmoke[1:5]


moredata <- cbind(lung,femalesmoke)

moredata[1:5,]


rm(list = ls())



