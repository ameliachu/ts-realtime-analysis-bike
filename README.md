# ts-realtime-analysis-bike
This project was created as a part of Forecasting Timeserie with Professor Hurvich at NYU Stern School of Business. In this project, we use a dataset of our choosing and attempt to fit an ARIMA-ARCH model. I collected Citi Bike Station Status data every minute for this project.

## citi-station-status.py
This script retrieves the status_status from the citibikenyc website and writes out the information in a json file. I set up a job on a micro ec2 cluster with crontab to collect this information every minute for a month.

## consolidate-realtime-collection-\*.ipynb
This notebook consolidates the json files collected into a smaller csv file. csvs are generated on the daily level.

## citi-bike-trimming-\*.ipynb
This notebook takes all of the csvs and generates a csv with only station 161 (LaGuardia Place) and datetime + Bikes Available. This makes it easier to perform analysis in R.
