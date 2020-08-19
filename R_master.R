person<-read.csv("movie_per_person",fileEncoding = "Shift_JIS")
install.packages("xlsx", dep=T);
library(xlsx)
x <- read.xlsx("/Users/naoki/Desktop/一橋/データセット/the-movies-dataset/movie_per_person.xlsx")

per<-movie_per_person
head(per)

per$"2011"

gbo <- read_excel("Desktop/一橋/データセット/GBO.xlsx")
View(gbo)
d3 <- merge(per,gbo,by="Country")
d3
View(d3)
gbo<-gbo %>% rename("2016" = "...14")
gbo_r<-gbo
gbo<-gbo[,c(-9)]
list(gbo)
install.packages("tidyr")
gbo_c<- gbo %>% gather(year,data,-Country)
View(gbo_c)
gbo_c<-gbo_c %>% rename(gbo=data)
movie <- movie_per_person %>% gather(year,data,-Country)
View(movie)
m_d3<- merge(gbo_c,movie, by.gbo_c=c("Country", "year")
             , by.movie=c("Country", "year"))


m_d3<-rename(m_d3,movie=data)

res<-lm(m_d3$gbo ~ m_d3$movie)
typeof(m_d3$gbo)
m_d3<-m_d3r
as.numeric(m_d3$gbo)
di
m_d3r<-m_d3

View(m_d3)
d3 <- d3 %>% gather(year,data,-Country)

d3na<-subset(m_d3,!(is.na(m_d3$gbo)))
d3nap<-subset(d3na,!(is.na(d3na$movie)))
d3na<-subset(m_d3,!(is.na(m_d3$gbo)))
d3nag<-filter(d3na,gbo!="..")
d3nagg<-filter(d3nag,movie!="..")
View(d3nagg)

res<-lm(d3nagg$gbo~d3nagg$movie)
summary(res)
d3nagg$gbo<-as.numeric(d3nagg$gbo)
d3nagg$movie<-as.numeric(d3nagg$movie)
