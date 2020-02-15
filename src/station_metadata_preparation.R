library(data.table)
library(RSQLite)
db_path = 'data/tripdata.db'
con <- dbConnect(RSQLite::SQLite(),db_path)
file_pattern = "%s%02d-citibike-tripdata.csv"
data_folder = 'data/'
years = 2018
months = 1:12
colClasses = c('gender'='NULL', 'birth year'='NULL', 'usertype'='NULL', 'bikeid'='NULL', 'tripduration'='NULL', 'starttime'='NULL', 'stoptime'='NULL',
               'end station id'='NULL', 'end station name'='NULL', 'end station longitude'='NULL','end station latitude'='NULL')
data = data.table()
for(year in years){
    for(month in months){
        filename =sprintf(file_pattern, year, month)
        filepath = paste0(data_folder,filename)
        if(grepl('.zip',filepath)){
               unzip(filepath, exdir=data_folder)
               filepath=gsub('.zip','',filepath)
        }
        if(file.exists(filepath)){
            print(paste("Reading: ", filepath))
            new_data = unique(fread(filepath, colClasses=colClasses, stringsAsFactors=FALSE))
            names(new_data) = gsub('start ','',names(data))
            names(new_data) = gsub(" ", "_", names(data))
            data = unique(rbind(data, new_data, fill=TRUE))
        }
    }
}
names(data) = gsub('start ','',names(data))
names(data) = gsub(" ", "_", names(data))
data = data[station_id != 'NULL',]
dbWriteTable(con, "stationdata",data, overwrite=T)
dbDisconnect(con)
