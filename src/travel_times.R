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

distance_matrix = mp_get_matrix(mp_matrix(origins='Cologne', destinations='Berlin', key=api_key))
routes = mp_get_routes(mp_directions(origin='Cologne', destination='Berlin', key=api_key))

