url_pattern = "https://s3.amazonaws.com/tripdata/"
file_pattern = "JC-%s%02d-citibike-tripdata.csv.zip"
data_folder = 'data/'
years = 2018
months = 1:12
for(year in years){
    for(month in months){
        filename =sprintf(file_pattern, year, month)
        filepath = paste0(data_folder,filename)
        while(!file.exists(filepath)){
            url = paste0(url_pattern, filename)
            tryCatch(download.file(url, filepath, method='wget'), error=function(e) print(e))
        }
    }
}
