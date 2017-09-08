# Load libraries
library(data.table)
library(tidyverse)
library(lubridate)
library(leaflet)

# Column classes
train_classes <- c("factor", "factor", "character", "character", "integer",
                   "numeric", "numeric", "numeric", "numeric", 
                   "factor", "numeric")

test_classes <- c("factor", "factor", "character", "integer",
                  "numeric", "numeric", "numeric", "numeric", "factor")

# Read in a training data set
train <- fread("train.csv", colClasses = train_classes)
test <- fread("test.csv", colClasses = test_classes)

summary(train)

sum(is.na(train))


# Reformat train dataset
train <- train %>%
  mutate(pickup_datetime = ymd_hms(pickup_datetime),
         dropoff_datetime = ymd_hms(dropoff_datetime),
         vendor_id = factor(vendor_id),
         passenger_count = factor(passenger_count))

test <- test %>%
  mutate(pickup_datetime = ymd_hms(pickup_datetime),
         vendor_id = factor(vendor_id),
         passenger_count = factor(passenger_count))


set.seed(412)

foo <- sample_n(train, 8e3)

leaflet(data = foo) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircleMarkers(~ pickup_longitude, ~pickup_latitude, radius = 1,
                   color = "blue", fillOpacity = 0.2)
  
leaflet(data = foo) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircleMarkers(~ dropoff_longitude, ~dropoff_latitude, radius = 1,
                   color = "red", fillOpacity = 0.2)


train %>%
  ggplot(aes(trip_duration)) +
  geom_histogram(fill = "red", bins = 150) +
  scale_x_log10() +
  scale_y_sqrt()


train %>%
  filter(pickup_datetime > ymd("2016-01-20") & pickup_datetime < ymd("2016-02-10")) %>%
  ggplot(aes(pickup_datetime)) +
  geom_histogram(fill = "red", bins = 120)


library(grid)

p1 <- train %>%
  group_by(passenger_count) %>%
  count() %>%
  ggplot(aes(passenger_count, n, fill = passenger_count)) +
  geom_col() +
  scale_y_sqrt() +
  theme(legend.position = "none")

p2 <- train %>%
  ggplot(aes(vendor_id, fill = vendor_id)) +
  geom_bar() +
  theme(legend.position = "none")

p3 <- train %>%
  ggplot(aes(store_and_fwd_flag)) +
  geom_bar() +
  theme(legend.position = "none") +
  scale_y_log10()

p4 <- train %>%
  mutate(wday = wday(pickup_datetime, label = TRUE)) %>%
  group_by(wday, vendor_id) %>%
  count() %>%
  ggplot(aes(wday, n, colour = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Day of the week", y = "Total number of pickups") +
  theme(legend.position = "none")

p5 <- train %>%
  mutate(hpick = hour(pickup_datetime)) %>%
  group_by(hpick, vendor_id) %>%
  count() %>%
  ggplot(aes(hpick, n, color = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Hour of the day", y = "Total number of pickups") +
  theme(legend.position = "none")

layout <- matrix(c(1,2,3,4,5,5),3,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, layout=layout)

#############################################################################
# Feature engineering
#############################################################################


library(geosphere)
library(forcats)

jfk_coord <- tibble(lon = -73.778889, lat = 40.639722)
la_guardia_coord <- tibble(lon = -73.872611, lat = 40.77725)

pick_coord <- train %>%
  select(pickup_longitude, pickup_latitude)
drop_coord <- train %>%
  select(dropoff_longitude, dropoff_latitude)
train$dist <- distCosine(pick_coord, drop_coord)
train$bearing = bearing(pick_coord, drop_coord)

train$jfk_dist_pick <- distCosine(pick_coord, jfk_coord)
train$jfk_dist_drop <- distCosine(drop_coord, jfk_coord)
train$lg_dist_pick <- distCosine(pick_coord, la_guardia_coord)
train$lg_dist_drop <- distCosine(drop_coord, la_guardia_coord)

train <- train %>%
  mutate(speed = dist/trip_duration*3.6,
         date = date(pickup_datetime),
         month = month(pickup_datetime, label = TRUE),
         wday = wday(pickup_datetime, label = TRUE),
         wday = fct_relevel(wday, c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")),
         hour = hour(pickup_datetime),
         work = (hour %in% seq(8,18)) & (wday %in% c("Mon","Tues","Wed","Thurs","Fri")),
         jfk_trip = (jfk_dist_pick < 2e3) | (jfk_dist_drop < 2e3),
         lg_trip = (lg_dist_pick < 2e3) | (lg_dist_drop < 2e3),
         blizzard = !( (date < ymd("2016-01-22") | (date > ymd("2016-01-29"))) )
  )

# Test dataset

pick_coord <- test %>%
  select(pickup_longitude, pickup_latitude)
drop_coord <- test %>%
  select(dropoff_longitude, dropoff_latitude)
test$dist <- distCosine(pick_coord, drop_coord)
test$bearing = bearing(pick_coord, drop_coord)

test$jfk_dist_pick <- distCosine(pick_coord, jfk_coord)
test$jfk_dist_drop <- distCosine(drop_coord, jfk_coord)
test$lg_dist_pick <- distCosine(pick_coord, la_guardia_coord)
test$lg_dist_drop <- distCosine(drop_coord, la_guardia_coord)

test <- test %>%
  mutate(#speed = dist/trip_duration*3.6,
         date = date(pickup_datetime),
         month = month(pickup_datetime, label = TRUE),
         wday = wday(pickup_datetime, label = TRUE),
         wday = fct_relevel(wday, c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")),
         hour = hour(pickup_datetime),
         work = (hour %in% seq(8,18)) & (wday %in% c("Mon","Tues","Wed","Thurs","Fri")),
         jfk_trip = (jfk_dist_pick < 2e3) | (jfk_dist_drop < 2e3),
         lg_trip = (lg_dist_pick < 2e3) | (lg_dist_drop < 2e3),
         blizzard = !( (date < ymd("2016-01-22") | (date > ymd("2016-01-29"))) )
  )





# Final cleaning
train <- train %>%
  filter(trip_duration < 22*3600,
         dist > 0 | (near(dist, 0) & trip_duration < 60),
         jfk_dist_pick < 3e5 & jfk_dist_drop < 3e5,
         trip_duration > 10,
         speed < 100)







##############
# TODO
##############




# Let's just plot a sample of the data
sample <- train %>%
  mutate(pickup_hour = hour(ymd_hms(pickup_datetime))) %>%
  sample_n(20000)

ggplot(sample, aes(x = pickup_hour, y = log(trip_duration))) +
  geom_point(position = "jitter",
             alpha = 0.25) +
  geom_smooth() +
  labs(x = "Hour of Pickup", 
       y = "Log of Trip Duration",
       title = "Trip Duration by Pickup Hour")

library(randomForest)

# rf_benchmark <- randomForest(trip_duration ~ vendor_id + passenger_count
#                              + pickup_longitude + pickup_latitude,
#                              data = sample,
#                              ntree = 100)

rf_benchmark <- randomForest(trip_duration ~ vendor_id + passenger_count
                             + pickup_longitude + pickup_latitude
                             + month + wday + hour
                             + dist + blizzard,
                             data = sample,
                             ntree = 100)

levels(test$passenger_count) <- levels(sample$passenger_count)

rf_prediction <- predict(rf_benchmark, test, type = "response") 

# Prepare the submission file and write it to the "Output" directory
submission_file <- data.frame(id = test$id,
                              trip_duration = rf_prediction)

write.csv(submission_file, "simple_submission_file2.csv", row.names=F)