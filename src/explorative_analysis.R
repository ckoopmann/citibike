options(width=150)
library(data.table)
library(ggplot2)
library(ggthemes)
library(RSQLite)

plot_directory = 'plots/'
db_path = 'data/tripdata.db'
con <- dbConnect(RSQLite::SQLite(),db_path)

ride_data = as.data.table(dbGetQuery(con, 'SELECT tripduration, usertype, birth_year, gender FROM tripdata'))


design = function(x) x + theme_few() + scale_fill_economist() + scale_colour_economist()

duration_cutoff = 60*60
share_above_cutoff = ride_data[,mean(tripduration>duration_cutoff)]
density_durations_by_type = design(ggplot(data=ride_data[tripduration<=duration_cutoff], aes(x=tripduration/60, fill=usertype, col=usertype)) +
                                   labs(title="Distribution of Trip Duration by Usertype", xlab="Tripduration [min]", ylab="Density")
                                   geom_density(alpha=0.5))
ggsave(density_durations_by_type, file = paste0(plot_directory, 'density_durations_by_type.jpeg')) 

