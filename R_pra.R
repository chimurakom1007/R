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





