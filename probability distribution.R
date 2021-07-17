getwd()
lung = read.csv(file ="C:/Users/Nowshad/Documents/LungCapData.csv")
View(lung)
attach(lung)

##probability distribution in R.

#p(x=3)

?dbinom
#calculating probability for binomial random varible using R
##dbinom means binomial distribution

# x = number of success , size = number of trail,
#prob = 	probability of success on each trial.

dbinom(x = 3,size = 20,prob=1/6)

#probability of success from 0 - 3
dbinom(x = 0:3,size = 20,prob=1/6)

#probability of success fromm 0-3 together

sum(dbinom(x = 0:3,size = 20,prob=1/6))

#another way to find probability x <=3
#pbinom(q, size, prob, lower.tail = TRUE, log.p = FALSE)

pbinom(q=3,size = 20,prob = 1/6,lower.tail = TRUE)



#calculating probability for poisson random varible using R

#here X follows a poisson distribution with a known rate of lamda x = 7
#x~poisson (x=7)

help(dpois)
dpois(x=4,lambda = 7)
dpois(x=0:4,lambda = 7)

#p(x<=4)

sum(dpois(x=0:4,lambda = 7))

#p(x<=4)

ppois(q = 4,lambda = 7,lower.tail = TRUE)

#p(x >=12)

ppois(q = 12,lambda = 7,lower.tail = FALSE)


#calculating probability for normal random varible using R

help("pnorm")

#p(x = 70)
dnorm(x = 70, mean = 75, sd = 5, log = FALSE)


#p(x <= 70)

pnorm(q = 70, mean = 75, sd = 5, lower.tail = TRUE)

#p(x => 70)

pnorm(q = 70, mean = 75, sd = 5, lower.tail = FALSE)


#p(z>=1)

pnorm(q = 1, mean = 0, sd = 1, lower.tail = FALSE)


#find Q1
qnorm(p=0.25,mean=75,sd=5,lower.tail = T)


x = seq(from = 55,to=95,by = 0.25)

probabiltyDensity = dnorm(x, mean = 75, sd = 5)



plot(x,probabiltyDensity)

plot(x,probabiltyDensity,type = "l")

plot(x,probabiltyDensity,type = "l",main = "Density vs X --mean = 75--SD=5",
xlab = "x",ylab = "probabilty Density",las = 1)
abline(v = 75)


rand = rnorm(n = 40,mean = 75, sd = 5)
rand

hist(rand)





##T - distribution using R

help(pt)

#t-stat = 2.3,df = 25

#one-sided pvalue.

#p(t>2.3)

pt(q = 2.3, df = 25, lower.tail =F)


#two sided value

pt(q = 2.3, df = 25,lower.tail =F) + pt(q = -2.3, df = 25,lower.tail =T)

pt(q = 2.3, df = 25,lower.tail =F)*2


#find t for 95% confidence
#value of t with 2.5% in each tail
qt(p = 0.025, df = 25, lower.tail = TRUE)


#help desk for probability of F distribution
help(pf)

#help desk for probability of Exponential distribution

help(pexp)




