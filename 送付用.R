
re <- kiroku
#必要なパッケージの読み込み

library(tidyverse)

kiroku3<-kiroku %>% rename(sokutei=患者名)


#kiroku3<-kiroku %>%

kiroku3<-kiroku3 %>%
  mutate(isna=str_detect(sokutei,"\\s")) %>%
  mutate(name=if_else(isna==FALSE,sokutei,NA_character_)) %>%
  fill(name)

#kiroku3$sokutei<-str_trim(kiroku$sokutei)

#kiroku3<-kiroku %>% mutate(sokutei=str_trim(sokutei))

kiroku3<-kiroku3 %>% 
  mutate(sokutei=str_trim(sokutei))

#kiroku3<- kiroku %>% mutate(yousu=if_else(isna==TRUE,sokutei,"admission"))

kiroku3 <- kiroku3 %>% 
  mutate(yousu=if_else(isna==TRUE,sokutei,"admission"))

#kiroku3 <- kiroku %>% select(-isna,-sokutei)

kiroku3 <- kiroku3 %>% 
  select(-isna,-sokutei)

#kiroku3<-kiroku %>% gather(-name,-yousu,key=day,value = status)

kiroku3<-kiroku3 %>% 
  gather(-name,-yousu,key=day,value = status)

#kiroku3<-kiroku %>% filter(!is.na(status))

kiroku3<-kiroku3 %>% 
  filter(!is.na(status))

#血圧
bpressure<-kiroku3 %>% filter(yousu=="血圧") %>%
  separate(col = "status",
           into = c("am","pm"),
           sep = "-") %>%
  separate(col = "am",
           into = c("am_down","am_up"),
           sep = "/") %>%
  separate(col = "pm",
           into = c("pm_down","pm_up"),
           sep = "/")


#脈拍
dokun<-kiroku3 %>% filter(yousu=="脈拍") %>%
  
  separate(col = "status",
           into = c("up","down"),
           sep = "-")

#呼吸回数
breath<-kiroku3 %>% 
  filter(yousu=="呼吸回数") %>%
  separate(col = "status",
           into = c("up","down"),
           sep = "-")

#食事
eat<-kiroku3 %>% 
  filter(yousu=="食事") %>%
  separate(col = "status",
           into = c("morning","lunch","dinner"),
           sep = "-")

bpressure
dokun
breath
eat

#ここからは元のスクリプトを削除したうえで追記しています
#横方向に並べたいというご要望だと思います。
#多分ですが、ご質問いただいたデータの形になってしまう
#理由として、yousu変数を含めてJoinされているからだと思います。

bpressure2 <- bpressure %>%
  mutate_at(
    vars(am_down, am_up, pm_down, pm_up),
    ~{as.numeric(str_trim(.))}
  ) %>% 
  rename(bp_am_d = am_down,
         bp_am_u = am_up,
         bp_pm_d = pm_down,
         bp_pm_u = pm_up) %>% 
  select(-yousu)

dokun2 <- dokun %>% 
  mutate(up   = if_else(up==""  ,NA_character_,up  )) %>% 
  mutate(down = if_else(down=="",NA_character_,down)) %>% 
  mutate(up = str_trim(up),
         down = str_trim(down)) %>% 
  mutate(up = as.numeric(up),
         down = as.numeric(down)) %>% 
  rename(pulse_u = up,
         pulse_d = down) %>% 
  select(-yousu)

#dokun2は次のようにすると楽に書けます
#(ややこしければ上のやり方でも問題ありません)
dokun2 <- dokun %>% 
  mutate_at(
    vars(up,down),
    ~{
      if_else(.=="",NA_character_,.) %>% 
        str_trim() %>% 
        as.numeric()
    }
  ) %>% 
  rename(pulse_u = up,
         pulse_d = down) %>% 
  select(-yousu)


breath2 <- breath %>% 
  mutate(up   = str_replace(up  ,"回","")) %>% 
  mutate(down = str_replace(down,"回","")) %>% 
  mutate(up   = str_trim(up)  ) %>% 
  mutate(down = str_trim(down)) %>%
  mutate(up   = as.numeric(up)) %>% 
  mutate(down = as.numeric(down)) %>% 
  rename(resp_u = up, 
         resp_d = down) %>% 
  select(-yousu)

#楽な書き方だけします
eat2 <- eat %>%
  mutate_at(
    vars(morning, lunch, dinner),
    ~{
      str_replace(.,"割","") %>% 
        str_trim() %>% 
        as.numeric()
    }
  ) %>% 
  rename(eat_m = morning,
         eat_l = lunch,
         eat_d = dinner)%>% 
  select(-yousu)

bpressure2
dokun2
breath2
eat2

#くっつけます

result <- bpressure2 %>% 
  left_join(dokun2 , by = c("name","day")) %>% 
  left_join(breath2, by = c("name","day")) %>% 
  left_join(eat2   , by = c("name","day"))

View(result)
