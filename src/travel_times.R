options(width=150)
library(data.table)
library(ggplot2)
library(ggthemes)
library(RSQLite)
library(xtable)
library(lubridate)
library(gridExtra)
library(mapsapi)

plot_directory = 'plots/'
api_key = Sys.getenv("api_key")
register_google(key=api_key)
db_path = 'data/tripdata.db'
con <- dbConnect(RSQLite::SQLite(),db_path)
design = function(x) x + theme_few() + scale_fill_economist() + scale_colour_economist()


ride_data = as.data.table(dbGetQuery(con, 'SELECT tripduration,
                                                usertype, 
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
                                           WHERE start_station_id <> end_station_id
                                           ORDER BY RANDOM() LIMIT 1000')) 

ride_data[, starttime:= as.POSIXct(starttime, origin='1970-01-01')]
distance_matrix = mp_get_matrix(mp_matrix(origins='Cologne', destinations='Berlin', key=api_key))

get_target_date <-function(date){
    date = with_tz(date, "EST")
    sys_date = with_tz(Sys.time(), "EST")
    wday_diff = as.difftime(wday(date) - wday(sys_date)+7, units="days")
    target_date = sys_date + wday_diff
    target_time = with_tz(as.POSIXct(target_date), "EST")
    hour(target_time) = hour(date)
    minute(target_time) = minute(date)
    return(target_time)}


get_travel_time <- function(start_longitude, start_latitude, end_longitude, end_latitude, starttime, mode){
    print(length(c(start_longitude, start_latitude)))
    response = mp_directions(origin=c(start_longitude, start_latitude), 
                            destination=c(end_longitude, end_latitude),  
                            departure_time=get_target_date(starttime),
                            mode=mode,
                            quiet=T,
                            key=api_key)
    routes = mp_get_routes(response)
    return(min(routes$duration_s))}

get_travel_time(ride_data[1]$start_longitude, ride_data[1]$start_latitude, ride_data[1]$end_longitude, ride_data[1]$end_latitude, ride_data[1]$starttime, "driving")

ride_data[,driving_time:= get_travel_time(start_longitude, start_latitude, end_longitude, end_latitude, starttime, "driving"), by = seq_len(nrow(ride_data))]
ride_data[,transit_time:= get_travel_time(start_longitude, start_latitude, end_longitude, end_latitude, starttime, "transit"), by = seq_len(nrow(ride_data))]
fwrite(ride_data, "data/travel_times.csv")

ride_data[, reldiff_driving := tripduration/driving_time -1]
ride_data[, reldiff_transit := tripduration/transit_time -1]

ride_data[, period:="other"]
ride_data[wday(starttime) %in% c(1,7), period:= "weekend"]
ride_data[!(wday(starttime) %in% c(1,7)) & hour(starttime) %in% c(5,6,7,8), period:= "morning rushhour"]
ride_data[!(wday(starttime) %in% c(1,7)) & hour(starttime) %in% c(16,17,18,19), period:= "evening rushhour"]

mean_diff_total = ride_data[driving_time > 0,
                            .(mean_diff_driving = mean(reldiff_driving),
                             mean_diff_transit = mean(reldiff_transit))]

mean_diff_hour = ride_data[driving_time > 0,
                           .(mean_diff_driving = mean(reldiff_driving),
                            mean_diff_transit = mean(reldiff_transit),
                            num=.N), 
                            by= .(usertype, hour=hour(starttime), weekend=period=="weekend")]

mean_diff_usertype = ride_data[driving_time > 0,
                               .(mean_diff_driving = mean(reldiff_driving),
                                mean_diff_transit = mean(reldiff_transit),
                                num=.N), 
                                by= usertype]

mean_diff_period = ride_data[driving_time > 0,
                               .(mean_diff_driving = mean(reldiff_driving),
                                mean_diff_transit = mean(reldiff_transit),
                                num=.N), 
                                by= .(usertype, period)]

mean_diff_period_long = melt(mean_diff_period, measure.vars=c("mean_diff_driving", "mean_diff_transit"))
mean_diff_period_long[,reference:=gsub("mean_diff_","",variable)]

plt_mean_diff_period = design(ggplot(mean_diff_period_long, aes(x=period)) + 
    labs(title="Average relative difference in travel time (n=1000)", x="Start Period", y="Rel Difference of travel time  to reference [%]")+
    geom_bar(stat='identity', position="dodge", width=0.5, alpha=0.5, aes(y=value*100, col=reference, fill=reference)) + 
    facet_grid(usertype ~ ., scales="free"))
ggsave(plt_mean_diff_period, file = paste0(plot_directory, 'plt_mean_diff_period.jpeg'), width=10, height=5) 

