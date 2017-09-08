library(dplyr)
library(leaflet)
library(geosphere)
library(lubridate)
library(ggplot2)

# Read in a training data set
train<-read.table("train.csv",sep=",",header=T,stringsAsFactors = F)


# Read in enreached data set (by L. Ciszak)
# trainProcessed<-read.table("train_processed.txt",sep="|",header=T,stringsAsFactors = F)


# Visualization of pickup places on a map (leaflet)

set.seed(1234)
foo <- sample_n(train, 8e3)

leaflet(data = foo) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircleMarkers(~ pickup_longitude, ~pickup_latitude, radius = 1,
                   color = "blue", fillOpacity = 0.3)



train$pickup_datetime <- ymd_hms(train$pickup_datetime)

train <- train %>%
  mutate(pickup_hour = hour(pickup_datetime),
         pickup_day = day(pickup_datetime),
         pickup_month = month(pickup_datetime),
         pickup_weekday = wday(pickup_datetime))


train$pickup_weekday <- factor(train$pickup_weekday,
                               levels = c(2, 3, 4, 5, 6, 7, 1),
                               labels = c("1", "2", "3", "4", "5", "6", "7"))
train$pickup_weekday <- as.integer(as.character(train$pickup_weekday))


train %>%
  count(pickup_weekday, pickup_hour) %>%
  ggplot() +
  geom_tile(aes(pickup_weekday, pickup_hour, fill = n), color = "white") +
  scale_fill_gradient(low = "darkred", high = "yellow") +
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(breaks = seq(0, 24, 3), trans = "reverse")


glimpse(train)