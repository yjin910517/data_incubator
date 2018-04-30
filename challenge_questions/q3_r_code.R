options(digits=10)
require(dplyr)

mt<-read.csv("MT_cleaned.csv",header=TRUE)
vt<-read.csv("VT_cleaned.csv",header=TRUE)

# Q2-1 Proportion of stops involving male drivers in Montana
ans21<-table(mt$driver_gender)["M"]/dim(mt)[1]


# Q2-2 How many more times likely are you to be arrested in Montana during a traffic stop 
# if you have out of state plates
chi.tab<-as.matrix(with(mt,table(out_of_state,is_arrested)))
chi.tab<-chi.tab[,c(2,1)]
prop<-chi.tab[,1]/apply(chi.tab,1,sum)

# "How many more times" is interpreted as the ratio of proportions between two groups minus 1
ans22<-prop["TRUE"]/prop["FALSE"]-1

# Q2-3 Chi-square
ans23<-prop.test(chi.tab)$statistic

# Q2-4 Speeding
ans24<-dim(filter(mt, grepl("Speeding",violation)))[1]/dim(mt)[1]

# Q2-5 DUI
mt.dui.prop<-dim(filter(mt, grepl("DUI",violation)))[1]/dim(mt)[1]
vt.dui.prop<-dim(filter(vt, grepl("DUI",violation)))[1]/dim(vt)[1]

# "How much more likely" is interpreted as the ratio of proportions between two groups minus 1
ans25<-mt.dui.prop/vt.dui.prop-1

# Q2-6 Predict manufacture year in 2020
mt$vehicle_year_num<-as.integer(as.character(mt$vehicle_year))
mt$year<-as.integer(substring(as.character(mt$stop_date),1,4))
mt.manu.yr<-mt %>% group_by(year) %>% summarise(n(),mean(vehicle_year_num,na.rm=TRUE))
names(mt.manu.yr)<-c("year","n","mean")
mt.manu.yr<-mt.manu.yr[!is.na(mt.manu.yr$year),]
lr<-lm(mean~year,data=mt.manu.yr)
new<-data.frame(year=c(2020))
ans26<-predict(lr,new)

# Q2-7 p-value
summary(lr)
ans27<-1-pf(summary(lr)$fstatistic[1],1,6)

# Q2-8 hours of traffic stop
mt$stop_hour<-substring(as.character(mt$stop_time),1,2)
vt$stop_hour<-substring(as.character(vt$stop_time),1,2)
stop_hour_combined<-as.data.frame(table(c(vt$stop_hour,mt$stop_hour)))[-1,]
names(stop_hour_combined)<-c("hour","Freq")
max_cnt<-stop_hour_combined[which.max(stop_hour_combined$Freq),]
min_cnt<-stop_hour_combined[which.min(stop_hour_combined$Freq),]
ans28<-max_cnt$Freq-min_cnt$Freq

# Q2-9 Area of counties in Montana

# Use google map state border as reference, the reasonable range of lon is 
# (-120,-100), range of lat is (40,50)
# boxplot reveals some outliers, which are removed
boxplot(lon~county_name,data=mt)
boxplot(lat~county_name,data=mt)

mt.area<-mt %>% filter(between(lon,-120,-100)) %>% 
  filter(between(lat,40,50)) %>% 
  group_by(county_name) %>% 
  summarise(n=n(),min_lon=min(lon),max_lon=max(lon),min_lat=min(lat),max_lat=max(lat),
            sd_lon=sd(lon,na.rm=TRUE),sd_lat=sd(lat,na.rm=TRUE))

mt.area<-mt.area[-1,]

# At 40 degree latitude, one degree of latitude =  111.03 km
# one degree of longitude =  85.39 km

mt.area$area_km<-mt.area$sd_lat*mt.area$sd_lon*111.03*85.39*3.1415926
ans29<-unlist(mt.area[which.max(mt.area$area_km),"area_km"])

