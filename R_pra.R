data(warpbreaks) 
attach(warpbreaks)
fc <- factor(tension) 

levels(fc)
names(warpbreaks)
d<-lm(warpbreaks)
summary(d)
table(fc)     





for(i in 1:100) { 
  x <- 10
  y <- x + i^2
  print(y)
}           
s <- 0
i <- 100
while (i > 0) {
  s <- s + i
  i <- i - 1
  print(i)
  print(s)
}

x<-15
if(x>10){
  x<-x^3
  print(x)
}else{
  print("Oh My Gods")
}
paste("Oh","MyGods")

paste("May I", "help you ?")

paste(month.abb, 1:12, c("st", "nd", "rd", rep("th",9)) )

x<-c(1,2,3,5)
as.character(x)
typeof(x)

a[,3]
a[[3]]
a<-rbind(c(1,23,4),c(2,3,4))
matrix(a)

getLogReturns<-function(x){
  d<-log(x)-log(x-1)
  return(d)
}
z<-getLogReturns(6)
print(z)


d<-function(x){
  x<-x^2+log(x)
 return(x)
}
d(3)


ref<-getXReturn(10)
ref
(1/2)^10

par(mfcol=c(1,1))

n<-500
q<-dbinom(n, 1000, 0.5)
hist(q)
geom_bar(mapping = aes(x=q,y=n))
getwd()
install.packages("sn")
library("sn")
xx=-30:30/10
plot(xx,dsn(xx,0,1,0),type="l",ylim=c(0,0.8))
lines(xx,dsn(xx,0,1,-2),lty=2)
lines(xx,dsn(xx,0,1,2),lty=3)
xx=-30:30/10
plot(xx,dsn(xx,0,1,0),type="l",ylim=c(0,0.8))
lines(xx,dsn(xx,0,1,-2),lty=2)
lines(xx,dsn(xx,0,1,2),lty=3)
source("/Users/naoki/Desktop/stock.d.R",encoding="cp932")
dsn2=function(x){sum(log(dsn(xxx,xi=x[1],omega=x[2],alpha=x[3])))}
xxx=stock.d[[2]]
op.result=optim(c(0,1,1),dsn2,control=(fnscale=-1))
op.result

xx2=-20:20/10
plot(xx2,dsn(xx2,op.result$par[1],op.result$par[2],op.result$par[3]),type="l")
par(family= "HiraKakuProN-W3")

stock.d.n=data.frame(stock.d$Date,scale(stock.d[,-1]))
stock.dist=dist(t(stock.d.n[,-c(1,5)]))
stock.dist
stock.clust=hclust(stock.dist)
plot(stock.clust)
stock.d2=stock.d[,-c(1,3)]-stock.d[,3]
lm.list=lapply(stock.d2[-1],function(x){lm(x~stock.d2[[1]])})
lm.df=t(data.frame(lapply(lm.list,function(x){x$coef})))
plot(hclust(dist(lm.df)))

Ito.d<-read.csv("/Users/naoki/Desktop/R/R/Ito.csv", fileEncoding = "Shift_JIS")
library(rpart)
data("kyphosis")
head(kyphosis)
wwfwf
