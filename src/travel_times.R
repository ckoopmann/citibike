options(width=150)
library(data.table)
library(ggplot2)
library(ggthemes)
library(RSQLite)
library(xtable)
library(lubridate)
library(gridExtra)
library(mapsapi)

api_key = Sys.getenv("api_key")
register_google(key=api_key)
db_path = 'data/tripdata.db'
con <- dbConnect(RSQLite::SQLite(),db_path)
design = function(x) x + theme_few() + scale_fill_economist() + scale_colour_economist()


ride_data = as.data.table(dbGetQuery(con, 'SELECT tripduration,
                                                starttime,
                                                start_stations.station_longitude as start_longitude,
                                                start_stations.station_latitude as start_latitude,
                                                start_stations.station_name as start_name,
                                                end_stations.station_longitude as end_longitude,
                                                end_stations.station_latitude as end_latitude,
                                                end_stations.station_name as end_name
                                           FROM tripdata
                                           JOIN stationdata start_stations ON tripdata.start_station_id = start_stations.station_id
                                           JOIN stationdata end_stations ON tripdata.end_station_id = end_stations.station_id
                                           ORDER BY RANDOM() LIMIT 100')) 

ride_data[, starttime:= as.POSIXct(starttime, origin='1970-01-01')]
distance_matrix = mp_get_matrix(mp_matrix(origins='Cologne', destinations='Berlin', key=api_key))

date = with_tz(ride_data[1]$starttime, "EST")
sys_date = with_tz(Sys.time(), "EST")
wday_diff = as.difftime(wday(date) - wday(sys_date)+7, units="days")
target_date = sys_date + wday_diff
target_time = with_tz(as.POSIXct(target_date), "EST")
hour(target_time) = hour(date)
minute(target_time) = minute(date)

response = mp_directions(origin=c(ride_data[1]$start_longitude, ride_data[1]$start_latitude), 
                                     destination=c(ride_data[1]$end_longitude, ride_data[1]$end_latitude),  
                                     departure_time=target_time,
                                     key=api_key)
routes = mp_get_routes(response)
