options(width=150)
library(data.table)
library(ggplot2)
library(ggthemes)
library(RSQLite)
library(xtable)
library(lubridate)
library(randomForest)
library(pROC)

plot_directory = 'plots/'
table_directory = 'tex/'
db_path = 'data/tripdata.db'
con <- dbConnect(RSQLite::SQLite(),db_path)
design = function(x) x + theme_few() + scale_fill_economist() + scale_colour_economist()


ride_data = as.data.table(dbGetQuery(con, 'SELECT tripduration, starttime, usertype, birth_year, gender, start_station_id, end_station_id FROM tripdata'))
ride_data[, age:= 2018-birth_year]
ride_data[, starttime:= as.POSIXct(starttime, origin='1970-01-01')]

ride_data[, weekend := wday(starttime) %in% c(1,7)]
ride_data[, morning_rushhour := hour(starttime) %in% c(5,6,7,8) & !weekend]
ride_data[, evening_rushhour := hour(starttime) %in% c(17,18,19) & !weekend]
ride_data[, gender_known := gender > 0] 
ride_data[, female := gender == 2] 

ride_data[,.(weekend=mean(weekend),
             morning_rushhour=mean(morning_rushhour),
             evening_rushhour=mean(evening_rushhour),
             female=mean(female),
             gender_known=mean(gender_known)), by=usertype]


station_data = as.data.table(dbGetQuery(con, 'SELECT * FROM stationdata'))
dbDisconnect(con)
start_stations= merge(ride_data[,.(Trips=.N, AvgDuration=mean(tripduration)/60),by=.(usertype, station_id = start_station_id)], station_data)
start_stations[,TotalTrips := sum(Trips), by=usertype]
start_stations[,TripShare := 100*Trips/TotalTrips]

num_stations = 50
top_start_stations = start_stations[order(Trips, decreasing=T),head(.SD,num_stations), by=usertype]
top_start_stations[,sum(TripShare),by=usertype]
top_customer_stations = unique(top_start_stations[usertype=='Customer',]$station_id)
top_subscriber_stations = unique(top_start_stations[usertype=='Subscriber',]$station_id)
length(intersect(top_customer_stations, top_subscriber_stations))

ride_data[,top_customer_station := start_station_id %in% top_customer_stations]
ride_data[,top_subscriber_station := start_station_id %in% top_subscriber_stations]

ride_data[,.(weekend=mean(weekend),
             morning_rushhour=mean(morning_rushhour),
             evening_rushhour=mean(evening_rushhour),
             top_subscriber_station = mean(top_subscriber_station),
             top_customer_station = mean(top_customer_station),
             female=mean(female),
             gender_known=mean(gender_known)), by=usertype]

ride_data[,usertype:=as.factor(usertype)]

rm(list=ls()[ls()!="ride_data"])

model_vars = c("usertype", "tripduration", "age", "gender_known", "female", "weekend", "morning_rushhour", "evening_rushhour", "top_subscriber_station", "top_customer_station")
ride_data = ride_data[,model_vars, with=F]

set.seed(123)
num_train_obs = 100000
num_test_obs = 10000
model_obs = sample(nrow(ride_data), num_train_obs+num_test_obs)
model_data = ride_data[model_obs]
rm(ride_data)
gc()

rf = randomForest(usertype ~ tripduration + age + gender_known + female + weekend + morning_rushhour+evening_rushhour+top_subscriber_station+top_customer_station,
                  data=model_data[1:num_train_obs])
rf.roc = roc(model_data[1:num_train_obs]$usertype, rf$votes[,2])
