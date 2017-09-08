
library(tidyverse)


inp1<-read.table("train.csv",sep=",",header=T,stringsAsFactors = F)

inp<-as.tibble(inp1)

#-- geo coordinates
trip.start <- inp %>% dplyr::select(id, pickup_longitude,pickup_latitude)
trip.end <- inp %>% dplyr::select(id, dropoff_longitude,dropoff_latitude)

library(sp)
library(geojsonio)
library(spatialEco)
coordinates(trip.start) <- ~pickup_longitude+pickup_latitude
coordinates(trip.end)<- ~dropoff_longitude+dropoff_latitude

#----------------------------------------------------------------
# reverse geocoding: borough
#----------------------------------------------------------------
boroughs<-geojson_read("Borough Boundaries.geojson",what="sp")
trip.start@proj4string<-boroughs@proj4string
trip.end@proj4string<-boroughs@proj4string


start.joined <- point.in.poly(trip.start, boroughs)
end.joined <- point.in.poly(trip.end, boroughs)

start<-data.frame(id=start.joined$id,start_boro_name=start.joined$boro_name,stringsAsFactors = F)
end<-data.frame(id=end.joined$id,end_boro_name=end.joined$boro_name,stringsAsFactors = F)

inp1<-inp %>% left_join(start) %>% left_join(end)
inp<-inp1

#----------------------------------------------------------------
# reverse geocoding: neighbourhood
#----------------------------------------------------------------
nbrh<-geojson_read("Neighborhood Tabulation Areas.geojson",what="sp")
trip.start@proj4string<-nbrh@proj4string
trip.end@proj4string<-nbrh@proj4string

start.joined <- point.in.poly(trip.start, nbrh)
end.joined <- point.in.poly(trip.end, nbrh)

start<-data.frame(
  id=start.joined$id,
  start_county=start.joined$county_fips,
  start_ntaname=start.joined$ntaname,
  stringsAsFactors = F)

end<-data.frame(
  id=end.joined$id,
  end_county=end.joined$county_fips,
  end_ntaname=end.joined$ntaname,
  stringsAsFactors = F)

inp<-inp %>% left_join(start) %>% left_join(end)

#----------------------------------------------------------------
# reverse geocoding: zipcode
#----------------------------------------------------------------
postal <- geojson_read("nyc-zip-code-tabulation-areas-polygons.geojson",what="sp")
trip.start@proj4string<-postal@proj4string
trip.end@proj4string<-postal@proj4string

start.joined <- point.in.poly(trip.start, postal)
end.joined <- point.in.poly(trip.end, postal)

start<-data.frame(id=start.joined$id,start_postal=start.joined$postalCode,start_poname=start.joined$PO_NAME,stringsAsFactors = F)
end<-data.frame(id=end.joined$id,end_postal=end.joined$postalCode,end_poname=end.joined$PO_NAME,stringsAsFactors = F)

inp<-inp %>% left_join(start) %>% left_join(end)

#-----------------------
# more massaging
#-----------------------
inp<- inp %>% mutate(start_day=substr(pickup_datetime,1,10),start_time=substr(pickup_datetime,12,19) )

#----------------------
# add weather
#----------------------
weather <- read.table("weather.tsv",sep="\t",header=T,stringsAsFactors = F)
w.full <- weather %>% 
  filter(boro_name=="Queens") %>% 
  mutate(boro_name="Bronx") %>% 
  rbind(weather) %>%
  rename(start_boro_name = boro_name)
inp <- inp %>% left_join(w.full)

#---------------------
# add sunrise/sunset
#---------------------
sunrise <- read.table("sunrise.txt",sep="\t",header=T,stringsAsFactors = F)
inp.tmp <- inp %>% left_join(sunrise)
inp<-inp.tmp

inp.tmp <- inp %>% 
  mutate(start_day=ymd(start_day)) 
inp<-inp.tmp

inp <- inp %>% 
  mutate(before_sunrise=ifelse(start_time<sunrise,1,0)) %>%
  mutate(after_sunset=ifelse(start_time>sunset,1,0)) %>%
  mutate(is_dark=sign(before_sunrise+after_sunset)) %>%
  mutate(before_twilight=ifelse(start_time<twilight_start,1,0)) %>%
  mutate(after_twilight=ifelse(start_time>twilight_end,1,0)) %>%
  mutate(outside_twilight=sign(before_twilight+after_twilight))

#---------------------
# add calendar events
#---------------------
inp <- inp %>% mutate(day_of_week = wday(start_day,label=T))

inp$holiday<-0
#Friday	January 01	New Years Day	
inp[inp$start_day=="2016-01-01",]$holiday<-1

#Monday	January 18	Martin Luther King Jr. Day	3rd Monday in January
inp[inp$start_day=="2016-01-18",]$holiday<-1

#Friday	February 12	Lincoln's Birthday	Connecticut, Illinois, Missouri, New Jersey, New York.

#Monday	February 15	Presidents' Day	3rd Monday in February. Not all states
inp[inp$start_day=="2016-02-15",]$holiday<-1

#Monday	May 30	Memorial Day	Last Monday in May
inp[inp$start_day=="2016-05-30",]$holiday<-1



#---------------------
# add accidents
#---------------------
accidents<-read.table("Austin_SES_Demo_NYC_Traffic.tsv",sep="\t", header=T, stringsAsFactors = F,fill=T)

library(stringr)
accidents <- accidents %>% mutate(TIME=str_pad(TIME,5,0,side = "left"))
accidents <- accidents %>% mutate(DATE=mdy(DATE))
acc_id<-(1:nrow(accidents))
accidents$acc_id<-acc_id
accidents <-accidents %>% filter(str_length(str_trim(BOROUGH))>0)
accidents <-accidents %>% mutate(boro_name=str_to_title(BOROUGH))

tmp.inp <- inp %>% dplyr::select(id,pickup_longitude,pickup_latitude,start_boro_name,start_day,start_time) %>%
  mutate(start_day=ymd(start_day))

hrs<-as.numeric(substr(tmp.inp$start_time,1,2))*60
mins<-as.numeric(substr(tmp.inp$start_time,4,5))
tmp.time<-hrs+mins
tmp.inp$time<-tmp.time

tmp.time<-as.numeric(substr(accidents$TIME,1,2))*60+as.numeric(substr(accidents$TIME,4,5))
accidents$acc_time<-tmp.time

inp$start_day<-ymd(inp$start_day)
accidents.start <- accidents %>% dplyr::select(boro_name,DATE,LATITUDE,LONGITUDE,acc_id,acc_time) %>%
  rename(start_boro_name=boro_name,start_day=DATE) %>%
  inner_join(tmp.inp)

accidents.start$td <- abs(accidents.start$acc_time-accidents.start$time)
accidents.start <- accidents.start %>% filter(td<=10)
write.table(accidents.start,"accidents_start.txt",sep="|",row.names=F)
accidents.start$dist<-0

dists<-accidents.start$dist
pb <- txtProgressBar(min = 0, max = nrow(accidents.start), initial = 0)
for(i in 1:nrow(accidents.start)){
  r<-accidents.start[i,]
  dists[i]<-dist_fun(r$LONGITUDE,r$LATITUDE,r$pickup_longitude,r$pickup_latitude)
  setTxtProgressBar(pb, i)
}



accidents.start <- accidents.start %>% mutate(TIME=hm(TIME),start_time=hms(start_time))

#-- add time difference: accidents withing 10 minutes of trip start

#-------------------------------
# add non NYC data
#-------------------------------
states<-geojson_read("gz_2010_us_040_00_5m.json",what="sp")
trip.start@proj4string<-states@proj4string
trip.end@proj4string<-states@proj4string


start.joined <- point.in.poly(trip.start, states)
end.joined <- point.in.poly(trip.end, states)

start<-data.frame(id=start.joined$id,start_state=start.joined$NAME,stringsAsFactors = F)
end<-data.frame(id=end.joined$id,end_state=end.joined$NAME,stringsAsFactors = F)

inp1<-inp %>% left_join(start) %>% left_join(end)
inp<-inp1

#-------- cleanup
inp[is.na(inp$start_boro_name),]$start_boro_name <- "OutsideNYC"

inp$start_ntaname<-as.character(inp$start_ntaname)
inp[is.na(inp$start_ntaname),]$start_ntaname <- "OutsideNYC"

inp$start_poname <- as.character(inp$start_poname)
inp[is.na(inp$start_poname),]$start_poname <- "OutsideNYC"

inp$start_postal<-as.character(inp$start_postal)
inp[is.na(inp$start_postal),]$start_postal<-"00000"

#--
inp$end_boro_name<-as.character(inp$end_boro_name)
inp[is.na(inp$end_boro_name),]$end_boro_name <- "OutsideNYC"

inp$end_ntaname<-as.character(inp$end_ntaname)
inp[is.na(inp$end_ntaname),]$end_ntaname <- "OutsideNYC"

inp$end_poname <- as.character(inp$end_poname)
inp[is.na(inp$end_poname),]$end_poname <- "OutsideNYC"

inp$end_postal<-as.character(inp$end_postal)
inp[is.na(inp$end_postal),]$end_postal<-"00000"


inp$start_county<-NULL
inp$end_county<-NULL

inp$start_state<-as.character(inp$start_state)
inp$end_state<-as.character(inp$end_state)

inp$outside<-0
inp[inp$start_boro_name=="OutsideNYC",]$outside<-1
inp[inp$end_boro_name=="OutsideNYC",]$outside<-1
#-------------------------------
# add start/destination distance
#-------------------------------
inp <- inp %>% 
  mutate(p2p=paste0(start_boro_name,'-',end_boro_name)) %>%
  mutate(same_boro = ifelse(start_boro_name==end_boro_name,1,0))

inp[inp$outside==1,]$same_boro<-0

library(geosphere)

dist_fun<-function(long1,lat1,long2,lat2){
  distm (c(long1, lat1), c(long2, lat2), fun = distGeo)
}

dists<-inp$id
for(i in 1:nrow(inp)){
  r<-inp[i,]
  dists[i] <- dist_fun(r$pickup_longitude,r$pickup_latitude,r$dropoff_longitude,r$dropoff_latitude) 
}

#--------------------- outliers
#-- duration outlier
inp$outlier<-0
inp[inp$trip_duration>mean(inp$trip_duration)+3*sd(inp$trip_duration),]$outlier<-1

inp<-inp %>% rename(time_outlier=outlier)

#-- distance outlier
inp$dist_outlier<-0
inp[inp$dist>mean(inp$dist)+3*sd(inp$dist),]$dist_outlier<-1

#1   572 records
inp[inp$dist>30000,]$dist_outlier<-1


#------------------------------
# exploration
#------------------------------
inp$dist<-as.numeric(inp$dist)
summary(inp$dist)

#------------------------------
# save data
#------------------------------
write.table(inp,"train_processed.txt",sep="|",row.names=F)
inp <- read.table("train_processed.txt",sep="|",stringsAsFactors =F,header=T)