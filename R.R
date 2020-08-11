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

x<-cbind(c(1,2,3),c(3,4,5),c(1,2,7))
x
b1<-matrix(c(3,1,1),3,1)
b2<-matrix(c(4,24,5),3,1)
b3<-matrix(c(4,6,5),3,1)
cbind(b1,b2,b3)
