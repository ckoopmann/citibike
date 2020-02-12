options(width=150)
library(data.table)
library(ggplot2)

library(RSQLite)
db_path = 'data/tripdata.db'
con <- dbConnect(RSQLite::SQLite(),db_path)

ride_data = as.data.table(dbGetQuery(con, 'SELECT tripduration FROM tripdata'))

duration_cutoff = 60*60
share_above_cutoff = ride_data[,mean(tripduration>duration_cutoff)]
hist_durations = ggplot(data=ride_data[tripduration<=duration_cutoff], aes(x=tripduration)) + geom_histogram(bins=100)
