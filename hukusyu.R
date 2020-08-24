kiroku

View(kiroku)

kiroku<-kiroku %>%  rename(sokutei=患者名)

kiroku <- kiroku %>% mutate(isna=str_detect(sokutei,"\\s")) %>% 
  mutate(status= if_else(isna==TRUE,sokutei,"admission"))

kiroku <- kiroku %>% mutate(name=str_detect(sokutei,"//s")) %>% 
  mutate(name= if_else(isna==FALSE,sokutei,NA_character_)) %>% 
  fill(name)

kiroku$status=str_trim(kiroku$status)

kiroku<-kiroku %>% select(-sokutei)
kiroku<-kiroku %>% select(-isna)

kiroku<- kiroku %>% gather(-name,-status,key = day,value = yousu)

pressure<- kiroku %>% filter(status=="血圧")
View(pressure)

p<-pressure
pressure<-p
pressure<-  pressure %>% filter(!is.na(yousu))

pressure <- pressure %>%  separate(col = "yousu",
                     into = c("am","pm"),
                     sep = "-") %>% 
  separate(col = "am",
           into = c("am-up","am-own"),
           sep = "/") %>% 
  separate(col = "pm",
           into = c("pm-up","pm-own"),
           sep = "/")

pressure<- pressure %>% 
  if_else(str_detect(.,"^\\s*$"),NA_character_,am-up)

