library(tidyverse)
tx <- readRDS("./data/txcensus.rds")

output <- tx %>% 
  #filter(SUMLEV=='140') %>% 
  select(SUMLEV,COUNTY,PLACE,TRACT,BLOCK,UA,UATYPE,ZCTA,SDELM,
                       SDSEC,SDUNI,POP100,HU100,INTPTLAT,INTPTLON,
                       P0010001,P0010002,P0010003,P0010004,P0010005,P0010006,
                       P0010007,P0010008,P0010009,P0010025,P0010047,P0010063,
                       P0010070) %>% 
  rename(tot_pop = P0010001,
         one_race = P0010002,
         two_races = P0010009,
         three_races = P0010025,
         four_races = P0010047,
         five_races = P0010063,
         six_races = P0010070) %>% 
  mutate(across(names(output)[c(12:13,16:28)],as.integer)) %>% 
  mutate(across(c(INTPTLAT,INTPTLON),as.double))

tractOutput <- output %>% 
  filter(SUMLEV=='140')

write_csv(tractOutput, file = "./data/txoutput.csv")

count(distinct(output["SUMLEV"]))

check <- tractOutput%>% 
  group_by(COUNTY) %>% 
  summarise(across(HU100,sum))

