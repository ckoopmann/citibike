options(width=150)
library(data.table)
library(ggplot2)
library(ggthemes)
library(RSQLite)
library(xtable)
library(lubridate)
library(randomForest)
library(pROC)
library(caret)
library(gridExtra)

db_path = 'data/tripdata.db'
con <- dbConnect(RSQLite::SQLite(),db_path)
design = function(x) x + theme_few() + scale_fill_economist() + scale_colour_economist()


ride_data = as.data.table(dbGetQuery(con, 'SELECT tripduration, starttime, usertype, birth_year, gender, start_station_id, end_station_id FROM tripdata'))
ride_data[, age:= 2018-birth_year]
ride_data[, starttime:= as.POSIXct(starttime, origin='1970-01-01')]

ride_data[, weekend := wday(starttime) %in% c(1,7)]
ride_data[, morning_rushhour := hour(starttime) %in% c(5,6,7,8) & !weekend]
ride_data[, evening_rushhour := hour(starttime) %in% c(17,18,19) & !weekend]
ride_data[, gender_known := gender > 0] 
ride_data[, female := gender == 2] 

ride_data[,.(weekend=mean(weekend),
             morning_rushhour=mean(morning_rushhour),
             evening_rushhour=mean(evening_rushhour),
             female=mean(female),
             gender_known=mean(gender_known)), by=usertype]


station_data = as.data.table(dbGetQuery(con, 'SELECT * FROM stationdata'))
dbDisconnect(con)
start_stations= merge(ride_data[,.(Trips=.N, AvgDuration=mean(tripduration)/60),by=.(usertype, station_id = start_station_id)], station_data)
start_stations[,TotalTrips := sum(Trips), by=usertype]
start_stations[,TripShare := 100*Trips/TotalTrips]

num_stations = 50
top_start_stations = start_stations[order(Trips, decreasing=T),head(.SD,num_stations), by=usertype]
top_start_stations[,sum(TripShare),by=usertype]
top_customer_stations = unique(top_start_stations[usertype=='Customer',]$station_id)
top_subscriber_stations = unique(top_start_stations[usertype=='Subscriber',]$station_id)
length(intersect(top_customer_stations, top_subscriber_stations))

ride_data[,top_customer_station := start_station_id %in% top_customer_stations]
ride_data[,top_subscriber_station := start_station_id %in% top_subscriber_stations]

ride_data[,usertype:=as.factor(usertype)]

rm(list=ls()[!ls()%in%c("ride_data", "design")])

model_vars = c("usertype", "tripduration", "age", "gender_known", "female", "weekend", "morning_rushhour", "evening_rushhour", "top_subscriber_station", "top_customer_station")
table_directory = 'tex/'
variables= data.frame(name=model_vars, type=c("Binary (Target)", "Integer", "Integer (Demographic)", "Binary (Demographic)", "Binary (Demographic)", "Binary","Binary","Binary","Binary","Binary"))
tex_table = xtable(variables)
# align(tex_table) = 'c|cccccc'
print(tex_table,
      file=paste0(table_directory,'variables.tex'),
      include.rownames=F,
      latex.environments=c("heightresizeenv"), 
      booktabs=T)


ride_data = ride_data[,model_vars, with=F]

set.seed(123)
num_train_obs = 100000
num_test_obs = 10000
model_obs = sample(nrow(ride_data), num_train_obs+num_test_obs)
model_data = ride_data[model_obs]
train_data = head(model_data, num_train_obs)
test_data = tail(model_data, num_test_obs)
rm(ride_data)
rm(model_data)
gc()

formula_full = formula(usertype ~ tripduration + age + gender_known + female + weekend + morning_rushhour+evening_rushhour+top_subscriber_station+top_customer_station)
formula_no_demographic = formula(usertype ~ tripduration + weekend + morning_rushhour+evening_rushhour+top_subscriber_station+top_customer_station)

roc_plot_data <- function(roc, modelname) {
    return(data.table(Model=modelname, Sensitivity=roc$sensitivities, Specificity=roc$specificities))
}

# Logistic regression
logit = glm(formula_full, data=train_data, family='binomial')
logit_predictions = predict(logit,newdata=test_data)
logit_roc = roc(test_data$usertype, logit_predictions)
logit_roc_plotdata = roc_plot_data(logit_roc, "Logistic Regression")
logit_roc_plotdata$Variables = "All"

tex_table = xtable(logit)
print(tex_table,
      file=paste0(table_directory,'logit_coeffs.tex'),
      latex.environments=c("heightresizeenv"), 
      booktabs=T)


logit_no_demographic = glm(formula_no_demographic, data=train_data, family='binomial')
logit_no_demographic_predictions = predict(logit_no_demographic,newdata=test_data)
logit_no_demographic_roc = roc(test_data$usertype, logit_no_demographic_predictions)
logit_no_demographic_roc_plotdata = roc_plot_data(logit_no_demographic_roc, "Logistic Regression")
logit_no_demographic_roc_plotdata$Variables = "Without Demographics"

# Random Forest
rf = randomForest(formula_full, data=train_data)
rf_predictions = predict(rf,newdata=test_data, type='prob')[,2]
rf_roc = roc(test_data$usertype, rf_predictions)
rf_roc_plotdata = roc_plot_data(rf_roc, "Random Forest")
rf_roc_plotdata$Variables = "All"

rf_no_demographic = randomForest(formula_no_demographic, data=train_data, family='binomial')
rf_no_demographic_predictions = predict(rf_no_demographic,newdata=test_data, type='prob')
rf_no_demographic_roc = roc(test_data$usertype, rf_no_demographic_predictions[,2])
rf_no_demographic_roc_plotdata = roc_plot_data(rf_no_demographic_roc, "Random Forest")
rf_no_demographic_roc_plotdata$Variables = "Without Demographics"

auc_table = data.frame(Model=c("Random Forest", "Logistic Regression", "Random Forest", "Logistic Regression"), 
                       Variables=c("All","All","Without Demographics", "Without Demographics"),
                       AUC = round(c(rf_roc$auc, logit_roc$auc, rf_no_demographic_roc$auc, logit_no_demographic_roc$auc), 3))

plot_directory = 'plots/'
roc_plot = design(ggplot(rbind(logit_roc_plotdata, rf_roc_plotdata, logit_no_demographic_roc_plotdata, rf_no_demographic_roc_plotdata), aes(y=Sensitivity, x=1-Specificity, col=Model, lty=Variables)) + geom_line()) +
    geom_abline(intercept=0, slope=1, lty='dashed') + 
    labs(x='1-Specificity (Share of wrongly classified Customers)',
        y= 'Sensitivity (Share of correctly classified Subscribers)') +
    annotation_custom(tableGrob(auc_table, theme=ttheme_minimal(), rows=NULL), xmin=0.5, xmax=0.9, ymin=0, ymax=0.25)
ggsave(roc_plot, file = paste0(plot_directory, 'roc_plot.jpeg'), width=10, height=5) 

