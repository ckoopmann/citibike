options(width=150)
library(data.table)
library(ggplot2)
library(ggthemes)
library(ggmap)
library(RSQLite)
library(xtable)
library(lubridate)

plot_directory = 'plots/'
table_directory = 'tex/'
db_path = 'data/tripdata.db'
con <- dbConnect(RSQLite::SQLite(),db_path)
design = function(x) x + theme_few() + scale_fill_economist() + scale_colour_economist()
api_key = Sys.getenv("api_key")
register_google(key=api_key)


ride_data = as.data.table(dbGetQuery(con, 'SELECT COUNT(bikeid) as Trips,
                                                SUM(tripduration) as TotalDuration,
                                                usertype,
                                                start_station_id,
                                                end_station_id
                                           FROM tripdata trips
                                            GROUP BY usertype, start_station_id, end_station_id')) 

station_data = as.data.table(dbGetQuery(con, 'SELECT * FROM stationdata'))

start_stations= merge(ride_data[,.(Trips=sum(Trips), AvgDuration=sum(TotalDuration)/sum(Trips*60)),by=.(usertype, station_id = start_station_id)], station_data)
start_stations[,TotalTrips := sum(Trips), by=usertype]
start_stations[,TripShare := 100*Trips/TotalTrips]

num_stations = 20
top_start_stations = start_stations[order(Trips, decreasing=T),head(.SD,num_stations), by=usertype]


map = get_map(location = "Midtown Manhattan, NYC", zoom = 13, source = "google", maptype='toner-lite')

start_station_map = design(ggmap(map, extent='device') + geom_point(aes(x=station_longitude, y=station_latitude, col=usertype, size=TripShare), data=top_start_stations)) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        # legend.position="none",
        plot.margin = unit(c(0, 0, -1, -1), 'lines')) +
  xlab('') +
  ylab('')

ggsave(start_station_map, file = paste0(plot_directory, 'start_station_map.jpeg')) 
