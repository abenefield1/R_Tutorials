q<- function (save="no", ...) {quit(save=save, ...)}
q

v1<-c(1,2,3)
v1
v2<-data.frame(c(4,5,6))
v2
vbound<-cbind(v1,v2)
vbound
str(vbound)# df


v1<-c(1,2,3)
v1
v2<-list(4,5,6)
v2
vbound<-cbind(v1,v2)
vbound
str(vbound) # list

library(DescTools)

v1<-c(1,2,3)
v1
v2<-c(4,5,6)
v2
vbound<-cbind(v1,v2)
vbound
str(vbound)


install.packages("DescTools")
library(DescTools)
attach(d.pizza)
deliver<-aggregate(count, by=list(area,driver), FUN=mean)
deliver



mylist<-list(1,2,"C",4,5)
mylist
unlist(mylist)


x<-as.Date("2018-10-01")
x
months(x)
attr(x)


library(stats)
?anova.lm

x<-c(1,2,3,4,5,6,7,8,9,NA)
xbar<-mean(x,na.rm=T)
xbar


class(d.pizza$temperature)
class(d.pizza[,"temperature"])
class(d.pizza[,"temperature", drop=F])
