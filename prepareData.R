library(tidyverse)
tx <- readRDS("./data/txcensus.rds")
clipped5 <- read_csv("./data/clipped_5.csv")
clipped1 <- read_csv("./data/clipped_1.csv")
intersect5 <- read_csv("./data/intersect_5.csv")
intersect1 <- read_csv("./data/intersect_1.csv")

# There is no station_no 18
# This station is only open for special events
stations <- read_csv("./data/Red Line Stations.csv")

output <- tx %>% 
  filter(STUSAB=="TX") %>% 
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
         six_races = P0010070) 

output <- output%>% 
  mutate(across(names(output)[c(12:13,16:28)],as.integer)) %>% 
  mutate(across(c(INTPTLAT,INTPTLON),as.double))

tractOutput <- output %>% 
  filter(SUMLEV=='140')

# calculate 1 mile radius coverage by all stations
# there are overlapping areas
# this is calculated using clipp method in QGIS
clip1 <- clipped1 %>% 
  mutate(percent = clipped_area/trac_area,
         radius = 1.0) %>% 
  left_join(tractOutput,by = c("COUNTYFP"="COUNTY","TRACTCE"="TRACT")) %>% 
  mutate(coverage_pop = as.integer(POP100*percent),
         coverage_hu = as.integer(HU100*percent)) %>% 
  group_by(radius) %>% 
  summarise(pop_coverage = sum(coverage_pop),
            hu_coverage = sum(coverage_hu))

# calculate 0.5 mile radius coverage by all stations
# there are overlapping areas
# this is calculated using clipp method in QGIS
clip5 <- clipped5 %>% 
  mutate(percent = clipped_area/trac_area,
         radius = 0.5) %>% 
  left_join(tractOutput,by = c("COUNTYFP"="COUNTY","TRACTCE"="TRACT")) %>% 
  mutate(coverage_pop = as.integer(POP100*percent),
         coverage_hu = as.integer(HU100*percent)) %>% 
  group_by(radius) %>% 
  summarise(pop_coverage = sum(coverage_pop),
            hu_coverage = sum(coverage_hu))

# calculate 1 mile radius coverage by individual station
# this is calculated using intersection method in QGIS
coverage1 <- intersect1 %>% 
  mutate(percent = intersect_area/trac_area,
         radius = 1.0) %>% 
  left_join(tractOutput,by = c("COUNTYFP"="COUNTY","TRACTCE"="TRACT")) %>% 
  mutate(coverage_pop = as.integer(POP100*percent),
         coverage_hu = as.integer(HU100*percent)) %>% 
  group_by(station_no, radius) %>% 
  summarise(tot_pop = sum(coverage_pop),
            tot_hu = sum(coverage_hu)) %>% 
  right_join(stations, by = "station_no") %>% 
  left_join(clip1, by = "radius") %>% 
  mutate(Latitude=Latitude+0.0000000001) 
# add noise to lat 
# so it will plot two cirbles for the same station in Power BI

# calculate 0.5 mile radius coverage by individual station
# this is calculated using intersection method in QGIS
coverage5 <- intersect5 %>% 
  mutate(percent = intersect_area/trac_area,
         radius = 0.5) %>% 
  left_join(tractOutput,by = c("COUNTYFP"="COUNTY","TRACTCE"="TRACT")) %>% 
  #select(c(1:11),POP100,HU100) %>% 
  mutate(coverage_pop = as.integer(POP100*percent),
         coverage_hu = as.integer(HU100*percent)) %>% 
  group_by(station_no, radius) %>% 
  summarise(tot_pop = sum(coverage_pop),
            tot_hu = sum(coverage_hu)) %>% 
  right_join(stations, by = "station_no")%>% 
  left_join(clip5, by = "radius")


# merge two files into one
# so that it is linked to the shapefile
coverages <- union_all(coverage1,coverage5)


# check data
# test5 < clip5
# test1 < clip1
test1 <- coverage1 %>% 
  group_by(radius) %>% 
  summarise(pop_coverage = sum(tot_pop),
            hu_coverage = sum(tot_hu))

test5 <- coverage5 %>% 
  group_by(radius) %>% 
  summarise(pop_coverage = sum(tot_pop),
            hu_coverage = sum(tot_hu))

count(distinct(output["SUMLEV"]))

# check county population summarized by adding tracts 
# are equal to the county pop100 in output
check <- tractOutput%>% 
  group_by(COUNTY) %>% 
  summarise(across(HU100,sum))

# write csv files
write_csv(tractOutput, file = "./data/txoutput.csv")
write_csv(coverages, file = "./data/redLineCoverages.csv")
