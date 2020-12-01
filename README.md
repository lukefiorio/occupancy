# Occupancy Detection
### edX Data Science Final Project - Independent Topic

This project uses data from the UCI Machine Learning Repository on a room's environmental measures to build an occupancy detection algorithm. The data can be found and downloaded here: 
- https://archive.ics.uci.edu/ml/machine-learning-databases/00357/occupancy_data.zip

Our goal is to build a model that predicts, as accurately as possible, whether a room is occupied based on various data readings (e.g., temperature, humidity, etc.). Specifically, we aim to build a model that detects occupancy with maximal **overall accuracy**.

**This repo contains 3 files:**
| File          | Description |
| ------------- | ----------- |
| occupancy.R   | The R code used to process the data and build our predictive models |
| occupancy.Rmd | The R Markdown file, which produces the full PDF report with narrative |
| occupancy.pdf | The final report, knitted from the RMD syntax |

**Problem Description:**

The data that we use to build and test our model contains approximately 20,500 observations taken over the course of about 2.5 weeks in February, 2015. Our data has 1 target variable (`occupancy`) and 6 predictors (`date`, `temperature`, `humidity`, `light`, `co2`, `humidity_ratio`).

Occupancy is a binary field that indicates whether the room is occupied at the time of observation. Each row in our data represents an observation made at a given time (`date`); observations are typically made in 1-minute increments. Each observation includes data readings from electronic sensors reporting information on the temperature, humidity, light, and CO<sub>2</sub> levels.  Ground truth occupancy was determined by time-stamped photos taken alongside the sensor readings.
