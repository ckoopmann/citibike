options(width=150)
library(data.table)
library(ggplot2)
library(ggthemes)
library(RSQLite)
library(xtable)

plot_directory = 'plots/'
table_directory = 'tex/'
db_path = 'data/tripdata.db'
con <- dbConnect(RSQLite::SQLite(),db_path)
design = function(x) x + theme_few() + scale_fill_economist() + scale_colour_economist()


ride_data = as.data.table(dbGetQuery(con, 'SELECT tripduration, usertype, birth_year, gender FROM tripdata'))

averages = ride_data[,.(usertype= 'All', trips=.N, age = mean(2018 - birth_year), tripduration=mean(tripduration)/60, 
                        unknown_gender = mean(gender==0), male  = mean(gender==1), female = mean(gender==2))]
averages_by_type = ride_data[,.(trips=.N, age = mean(2018 - birth_year), tripduration=mean(tripduration)/60, 
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
align(tex_table) = 'c|cccccc'
print(tex_table,
      hline.after=c(-1,0,2,3),
      file=paste0(table_directory,'averages.tex'),
      include.rownames=F,
      latex.environments=c("myresizeenv"), 
      booktabs=T)

duration_cutoff = 60*60
above_cutoff = ride_data[,mean(tripduration>duration_cutoff)]
density_durations_by_type = design(ggplot(data=ride_data[tripduration<=duration_cutoff], aes(x=tripduration/60, fill=usertype, col=usertype)) +
                                   labs(title="Distribution of Trip Duration by Usertype", x="Duration [min]", y="Density")+
                                   geom_density(alpha=0.5))
ggsave(density_durations_by_type, file = paste0(plot_directory, 'density_durations_by_type.jpeg'), width=10, height=5) 


