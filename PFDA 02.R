getwd()
lung = read.csv(file ="C:/Users/Nowshad/Documents/LungCapData.csv")

View(lung)


lungmean <-lung[,1:3]

View(lungmean)

?apply
Avg <- apply(X =lungmean,MARGIN = 2,FUN = mean)

Avg <- apply(X =lungmean,MARGIN = 2,FUN = mean,na.rm = TRUE)


colMeans(lungmean,na.rm = TRUE)

max <- apply(X =lungmean,MARGIN = 2,FUN = max,na.rm = TRUE)

min <- apply(X =lungmean,MARGIN = 2,FUN = min,na.rm = TRUE)



#calculating 20th and 80th percentile 


p <- apply(X =lungmean,MARGIN = 2,FUN = quantile,probs=c(.20,.80),na.rm = TRUE)



p <- apply(X =lungmean,MARGIN = 2,FUN = plot,type ="l")



rowSums(lungmean,na.rm=TRUE)







naturalFriendly = lung %>%select(Age,Height,Smoke) %>% filter(Smoke %in% c("yes","no"))%>%
  mutate(smoke1 = recode(Smoke,yes = 1,no = 2))







