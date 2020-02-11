options(width=150)
library(data.table)
library(ggplot2)

test_data_path = 'data/202001-citibike-tripdata.csv'
test_data = fread(test_data_path, colClasses=c('starttime'='POSIXct', 'stoptime'='POSIXct'), stringsAsFactors=TRUE)
