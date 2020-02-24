# Source Code
This directory contains the **R** source code used to obtain all results presented in the slides.

## Contents
The repository contains the following files:
* **data_download.R** Downloads monthly csv files of citibike data saves them in data directory
* **data_preparation.R** Reads in monthly csv files and saves data on individual trips to SQLLite database table.
* **station_metadata_perparation.R** Reads in monthly csv files and saves metadata of citibike stations to separate database table.
* **explorative_analysis_user.R** Generates the tables and plots presented in the explorative analysis of user demographics and trip times.
* **explorative_analysis_network.R** Generates the tables and plots presented in the explorative analysis of station data.
* **classification_models.R** Trains logistic regression / random forest model to separate customers from subscribers.
* **travel_times.R** Compares citibike travel times to reference routes provided by google maps.
* **collision_data.R** Performs some exploratory analysis of NYPD motor vehicle collision [data](https://data.cityofnewyork.us/Public-Safety/Motor-Vehicle-Collisions-Crashes/h9gi-nx95).

## Reproducibility
When reproducing the results following steps need to be followed:
* Install necesssary R packages that are loaded in any of the scripts.
* Create **plots** and **data** directories at the top level of this repository or change respective paths in the script
* Create google maps api key and add it as a environment variable **api_key**
* Scripts need to be run in above order with working directory set to top level of this repository
