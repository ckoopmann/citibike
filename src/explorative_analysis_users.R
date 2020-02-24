options(width=150)
library(data.table)
library(ggplot2)
library(ggthemes)
library(RSQLite)
library(xtable)
library(lubridate)

plot_directory = 'plots/'
table_directory = 'tex/'
db_path = 'data/tripdata.db'
con <- dbConnect(RSQLite::SQLite(),db_path)
design = function(x) x + theme_few() + scale_fill_economist() + scale_colour_economist()

counts = as.data.table(dbGetQuery(con, 'SELECT COUNT(DISTINCT bikeid) AS Bikes, COUNT(DISTINCT start_station_id) as stations FROM tripdata'))

ride_data = as.data.table(dbGetQuery(con, 'SELECT tripduration, starttime, usertype, birth_year, gender FROM tripdata'))
ride_data[, age:= 2018-birth_year]
ride_data[, starttime:= as.POSIXct(starttime, origin='1970-01-01')]

averages = ride_data[,.(usertype= 'All', trips=.N, age = mean(age), tripduration=mean(tripduration)/60, 
                        unknown_gender = mean(gender==0), male  = mean(gender==1), female = mean(gender==2))]
averages_by_type = ride_data[,.(trips=.N, age = mean(age), tripduration=mean(tripduration)/60, 
                                unknown_gender = mean(gender==0), male  = mean(gender==1), female = mean(gender==2)),
                            by=usertype]
averages = rbind(averages, averages_by_type)
averages[,male := male/(1-unknown_gender)]
averages[,female := female/(1-unknown_gender)]
averages[, male:=100*male]
averages[, female:=100*female]
averages[, unknown_gender:=100*unknown_gender]
averages = averages[order(-usertype)]
averages[, trips := round(trips / 1e6, 1)]
names(averages) = c("Type", "Trips[mio]", "Age[y]", "Duration[min]", "Unknown[%]", "Male[%]", "Female[%]")
tex_table = xtable(averages)
# align(tex_table) = 'c|cccccc'
print(tex_table,
      hline.after=c(-1,0,2,3),
      file=paste0(table_directory,'averages.tex'),
      include.rownames=F,
      latex.environments=c("myresizeenv"), 
      booktabs=T)

averages_knowngender = ride_data[gender != 0,.(usertype= 'All', trips=.N, age = mean(age), tripduration=mean(tripduration)/60, 
                        male  = mean(gender==1), female = mean(gender==2))]
averages_knowngender_by_type = ride_data[gender != 0,.(trips=.N, age = mean(age), tripduration=mean(tripduration)/60, 
                                male  = mean(gender==1), female = mean(gender==2)),
                            by=usertype]
averages_knowngender = rbind(averages_knowngender, averages_knowngender_by_type)
averages_knowngender[,male := male/(1-unknown_gender)]
averages_knowngender[,female := female/(1-unknown_gender)]
averages_knowngender[, male:=100*male]
averages_knowngender[, female:=100*female]
averages_knowngender = averages_knowngender[order(-usertype)]
averages_knowngender[, trips := round(trips / 1e6, 1)]
names(averages_knowngender) = c("Type", "Trips[mio]", "Age[y]", "Duration[min]", "Male[%]", "Female[%]")
tex_table = xtable(averages_knowngender)
# align(tex_table) = 'c|cccccc'
print(tex_table,
      hline.after=c(-1,0,2,3),
      file=paste0(table_directory,'averages_knowngender.tex'),
      include.rownames=F,
      latex.environments=c("myresizeenv"), 
      booktabs=T)

duration_cutoff = 90*60
above_cutoff = ride_data[,mean(tripduration>duration_cutoff)]
density_durations_by_type = design(ggplot(data=ride_data[tripduration<=duration_cutoff], aes(x=tripduration/60, fill=usertype, col=usertype)) +
                                   labs(title="Distribution of Trip Duration by Usertype", x="Duration [min]", y="Density")+
                                   geom_density(alpha=0.5))
ggsave(density_durations_by_type, file = paste0(plot_directory, 'density_durations_by_type.jpeg'), width=10, height=5) 


binwidth = 1
histogram_hour_by_type = design(ggplot(data=ride_data, aes(x=hour(starttime), fill=usertype, col=usertype)) +
                                   labs(title="Hourly distribution of trips by Usertype", x="Hour of the Day", y="Share")+
                                   geom_histogram(aes(y=binwidth*..density..),binwidth=binwidth, alpha=0.5, position='identity'))
ggsave(histogram_hour_by_type, file = paste0(plot_directory, 'histogram_hour_by_type.jpeg'), width=10, height=5) 

binwidth = 1
histogram_weekday_by_type = design(ggplot(data=ride_data, aes(x=wday(starttime), fill=usertype, col=usertype)) +
                                   labs(title="Daily ~Distribution of trips by Usertype", x="Weekday [1=Sun]", y="Share")+
                                   geom_histogram(aes(y=binwidth*..density..),binwidth=binwidth, alpha=0.5, position='identity'))
ggsave(histogram_weekday_by_type, file = paste0(plot_directory, 'histogram_weekday_by_type.jpeg'), width=10, height=5) 



age_cutoff = 90
binwidth = 1
above_cutoff = ride_data[,mean(age>age_cutoff)]
histogram_age_by_type = design(ggplot(data=ride_data[age<=age_cutoff], aes(x=age, fill=usertype, col=usertype)) +
                                   labs(title="Distribution of user age by Usertype", x="age [years]", y="Share")+
                                   geom_histogram(aes(y=binwidth*..density..),binwidth=binwidth, alpha=0.5, position='identity'))
ggsave(histogram_age_by_type, file = paste0(plot_directory, 'histogram_age_by_type.jpeg'), width=10, height=5) 


above_cutoff = ride_data[,mean(age>age_cutoff)]
ride_data[, gender_data:='Known']
ride_data[gender==0, gender_data:='Unknown']
histogram_age_by_type_gender = design(ggplot(data=ride_data[age<=age_cutoff], aes(x=age, fill=usertype, col=usertype)) +
                                   labs(title="Distribution of user age by Usertype", x="age [years]", y="Share")+
                                   geom_histogram(aes(y=binwidth*..density..),binwidth=binwidth, alpha=0.5, position='identity')+
                                   facet_grid(gender_data~., scales='free'))
ggsave(histogram_age_by_type_gender, file = paste0(plot_directory, 'histogram_age_by_type_gender.jpeg'), width=10, height=5) 


