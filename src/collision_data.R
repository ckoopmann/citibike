options(width=150)
library(data.table)
library(ggplot2)
library(ggthemes)
library(ggmap)
library(xtable)
library(lubridate)

plot_directory = 'plots/'
design = function(x) x + theme_few() + scale_fill_economist() + scale_colour_economist()
collision_data = fread('data/Motor_Vehicle_Collisions_-_Crashes.csv')
names(collision_data) = tolower(gsub(" ", "_", names(collision_data)))

borough_crashes = collision_data[,.(cyclists_killed = sum(number_of_cyclist_killed),
                  cyclists_injured = sum(number_of_cyclist_injured)),
                by = .(borough)]

collision_data[, crash_hour:=as.numeric(gsub(":.*", "",crash_time))]
hourly_crashes = collision_data[,.(cyclists_killed = sum(number_of_cyclist_killed),
                  cyclists_injured = sum(number_of_cyclist_injured)),
                by = .(crash_hour)]

hourly_crashes[order(crash_hour),]

api_key = Sys.getenv("api_key")
register_google(key=api_key)

collision_data[,night_time := crash_hour < 6 | crash_hour >21] 
collision_data[,crash_date := as.Date(crash_date, format="%m/%d/%Y")]
collision_data[,crash_wday := wday(crash_date)]
collision_data[,weekend := crash_wday %in% c(1,7)]
collision_data[,rush_hour := !weekend & crash_hour %in% c(7,8,9,16,17,18)]
manhattan_crash_data = collision_data[ borough=="MANHATTAN" & number_of_cyclist_injured>0]
manhattan_crash_data[,crossing := paste(on_street_name, cross_street_name, sep="/")]

dangerous_crossings = manhattan_crash_data[,.(injuries=sum(number_of_cyclist_injured), longitude=median(longitude,na.rm=T), latitude=median(latitude,na.rm=T)), by=crossing]
dangerous_crossings = dangerous_crossings[order(injuries, decreasing=T),]

map = get_map(location = "Midtown Manhattan, NYC", zoom = 12, source = "google", maptype='toner-lite')

crash_map = ggmap(map, extent='device') + geom_point(col="dodgerblue3",aes(x=longitude, y=latitude, size=injuries), data=dangerous_crossings[2:31]) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        # legend.position="none",
        plot.margin = unit(c(0, 0, -1, -1), 'lines')) +
  xlab('') +
  ylab('')

ggsave(crash_map, file = paste0(plot_directory, 'crash_map.jpeg')) 
