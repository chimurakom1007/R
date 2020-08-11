core.d<-read.csv("/Users/naoki/Desktop/data02_win.csv", fileEncoding = "Shift_JIS")
View(core.d)


core.d<-rename(core.d,age=年齢,sex=性別,no1=設問1,no2=設問2,no3=設問3,no4=設問4,no5=設問5)

apply(core.d[4:8],1,sum)
hist(core.d[4:8])
plot(core.d[4:8])
x<-rnorm(10000,20,50)
hist(x)
par(1)
head(core.d)
hist(core.d$no1,col="#993435")

hist(core.d$no1, breaks = seq(0,5,1),col="#993435",main = "NO1")

s_no1<-sd(core.d$no1)
m_no1<-mean(core.d$no1)

n<-nrow(core.d)
z<-(m_no1-3.5)/(s_no1/sqrt(n))
z
n1<-mean(core.d[,4])
n2<-mean(core.d[,5])
n3<-mean(core.d[,6])
n4<-mean(core.d[,7])
n5<-mean(core.d[,8])

mm<-c(3.5,2.5,4.2,3.2,3.7)

zz<-rep(0,5)

for(i in 1:5){
  zz[i]<-(mean(core.d[,3+i])-mm[i])/(sd(core.d[,3+i])/sqrt(n))
}

x <- rnorm(1000,0,1)
y <- rnorm(1000,0,1)
typeof(x)
par(mfrow = c(1, 1))
plot(x, y)               # 範囲は自動で決まる(xlim=c(1,10)を指定した場合と同じ)
plot(x, y, ylim=c(-4,4),xlim=c(-4,4))
qqplot(x,y)

data("economics")
economics

x<-economics$psavert
x1<-economics$unemploy
y<-economics$uempmed
View(economics)
b<-cov(x,y)/var(x)
a<-mean(y)-b*mean(x)
plot(x,y)
cor(x,y)
abline(a,b)
res<-lm(y~x+x1)
res
summary(res)
cor(x,x1)
cor(y,x)
cor(y,x1)
7 %/% 5
7 %% 2
-4.2:8.2
3-4.2:8
-4.2:8.2
4-1:8
-40:40/40

x = -20:20/20
x
plot(x,log(1+x),type="l")
xx= -10:10/10
points(xx,xx-xx^2/2+xx^3/3)

points(x,x^2)

x = NULL
 x
 is.null(x)
 c(x, 1:5)
 c(1:5,10:15)

a1<-c(3.1,1)
a2<-c(4,24,5)
a3<-c(9,9,3)
wqd
x<-cbind(c(1,2,3),c(3,4,5),c(1,2,7))
x
b1<-matrix(c(3,1,1),3,1)
b2<-matrix(c(4,24,5),3,1)
b3<-matrix(c(4,6,5),3,1)
cbind(b1,b2,b3)

names(core.d)
length(core.d)
View(core.d)
dim(core.d)

core.d[,c(2,3)]
core.d[,c(-2:-3)]
core.d[,-2:-3]
diff(core.d$no1)

par(mfcol=c(1,1))
barplot(table(core.d$no1[[1]]))
hist(core.d$no1,breaks = seq(0,5,1),col = "red")
x11()
plot(as.factor(core.d$sex[,1]))
qqnorm(scale(r1))
core.2d<-read.csv("/Users/naoki/Desktop/data.csv", fileEncoding = "Shift_JIS")
view(core.2d)

qqnorm(scale(r2))
qqnorm(r2)
abline(0,1)
hist(r2,freq=F,breaks=seq(-0.1,0.1,by=0.01))
r1=diff(data2.d[,1])/data2.d[-121,1]
r2=diff(log(data2.d[,1]))
data2.d=core.2d[,-1]
data.d<-core.2d
hist(r2,freq=F,breaks=seq(-0.1,0.1,by=0.01))
x=runif(1000)
x
y=qnorm(x)
qqnorm(y)
abline(0,1)

source("/Users/naoki/Desktop/test.d.R",encoding="cp932")
ls()
rm(xx)
rm(yy)
attach(test.d)
help(attach)
bb1=cov(xx,yy)/var(xx)
aa1=mean(yy)-bb1*mean(xx)
bb2=(t(xx)%*%yy-length(xx)*mean(xx)*mean(yy))/(sum(xx^2)-length(xx)*mean(xx)^2)
aa2=mean(yy)-bb2*mean(xx)

jj=function(par){
  sum((test.d$yy-par[1]-par[2]*test.d$xx)^2)
}
optim(c(0,1),fn=jj)
help(optim)

x1 <- x <- seq(-3,3,0.1)
x2 <- seq(7,13,0.1)
y <- x1^2+(x2-10)^2
#求めたいパラメータはベクトルで与える
bar <- function(x){
  (x[1])^2+(x[2]-10)^2
}

optim(c(-1,1),bar)

par(mfcol=c(1,2))
beta=NULL
for(i in 1:100){
  xx=runif(50,-1,1)
  ee=rnorm(50)
  yy=10+0.5*xx+ee
  beta[i]=cov(xx,yy)/var(xx)}

names(economics)

hist(beta,freq=F)
dim(economics)
install.packages("rgl")                # パッケージのインストール
library(rgl)                           # パッケージの呼び出し
rgl.open()                            # デバイスの起動
example(rgl.surface)                   # 作図
for(i in 1:360) rgl.viewpoint(i,i/4)   # 図形の回転