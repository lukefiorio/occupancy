
#### install/load packages ####
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")

#### set options ####
options(digits = 5)
options(pillar.sigfig = 6) # show more sig fig in group_by

#### download zipped data and combine datasets ####

# retreive the data from the UCI ML repository
dl <- tempfile()
download.file(
  "https://archive.ics.uci.edu/ml/machine-learning-databases/00357/occupancy_data.zip",
  dl)

# specify column names
col_names <- c("index", "date", "temperature", "humidity", 
               "light", "co2", "humidity_ratio", "occupancy")

# read in each of the downloaded data files
data_1 <- fread(text = readLines(unzip(dl, "datatraining.txt")),
                sep = ",", header = FALSE, skip = 1,
                col.names = col_names)
data_2 <- fread(text = readLines(unzip(dl, "datatest.txt")),
                sep = ",", header = FALSE, skip = 1,
                col.names = col_names)
data_3 <- fread(text = readLines(unzip(dl, "datatest2.txt")),
                sep = ",", header = FALSE, skip = 1,
                col.names = col_names)

# combine the datasets and drop the index column
occupancy <- within(rbind(data_1, data_2, data_3), rm(index))

# set seed and set aside 20% of occupancy data for Validation set
set.seed(1, sample.kind="Rounding") # set seed

# split the data into training & validation
test_index <- createDataPartition(
  y = occupancy$occupancy,
  times = 1, p = 0.2, list = FALSE)
train_set <- occupancy[-test_index,] # train data
test_set <- occupancy[test_index,] # validation data

# remove temporary data
rm(dl, col_names, data_1, data_2, data_3, occupancy, test_index)


#### data exploration ####

# summary info
format(nrow(train_set), big.mark=",", trim=TRUE) # nbr of rows
min(date(train_set$date)) # date of first observation
max(date(train_set$date)) # date of last observation
mean(train_set$occupancy) # prevalence
sapply(train_set, function(col) sum(is.na(col))) # check for N/A

# relationships between predictors and target

# temperature
fig_1 <- 
  train_set %>%
  mutate(rnd_temp = round(temperature/5, digits=1)*5) %>%
  group_by(rnd_temp) %>%
  summarize(occupancy = mean(occupancy)) %>%
  ggplot(aes(rnd_temp, occupancy)) +
  geom_bar(stat='identity', color = 'black') +
  ggtitle("Figure 1: Occupancy Prevalence\nby Temperature (\u00B0C)") +
  xlab("Temperature (\u00B0C)") +
  scale_y_continuous(name = "Occupancy", limits = c(0, 1)) +
  theme(plot.margin = unit(c(1,0.5,0,0.5),"cm"))

# humidity
fig_2 <- 
  train_set %>%
  mutate(rnd_humid = round(humidity/2, digits=0)*2) %>%
  group_by(rnd_humid) %>%
  summarize(occupancy = mean(occupancy)) %>%
  ggplot(aes(rnd_humid, occupancy)) +
  geom_bar(stat='identity', color = 'black') +
  ggtitle("Figure 2: Occupancy Prevalence\nby Humidity") +
  xlab("Relative Humidity") +
  scale_y_continuous(name = "Occupancy", limits = c(0, 1)) +
  theme(plot.margin = unit(c(1,0.5,0,0.5),"cm"))

# display temperature & humidity charts
grid.arrange(fig_1, fig_2, ncol=2)

# light
fig_3 <- 
  train_set %>%
  mutate(rnd_light = round(light/200, digits=0)*200) %>%
  group_by(rnd_light) %>%
  summarize(occupancy = mean(occupancy)) %>%
  ggplot(aes(rnd_light, occupancy)) +
  geom_bar(stat='identity', color = 'black') +
  ggtitle("Figure 3: Occupancy Prevalence\nby Light level (Lux)") +
  xlab("Lighting (Lux)") +
  scale_y_continuous(name = "Occupancy", limits = c(0, 1)) +
  theme(plot.margin = unit(c(1,0.5,0,0.5),"cm"))

# subscript the `2` in "CO2"
co2_label <- expression(paste(CO[2]," level (ppm)"))

# CO2
fig_4 <- 
  train_set %>%
  mutate(rnd_co2 = round(co2/100, digits=0)*100) %>%
  group_by(rnd_co2) %>%
  summarize(occupancy = mean(occupancy)) %>%
  ggplot(aes(rnd_co2, occupancy)) +
  geom_bar(stat='identity', color = 'black') +
  ggtitle("Figure 4: Occupancy Prevalence\nby CO2 level (ppm)") +
  xlab(co2_label) +
  scale_y_continuous(name = "Occupancy", limits = c(0, 1)) +
  theme(plot.margin = unit(c(1,0.5,0,0.5),"cm"))

# display light & CO2 charts
grid.arrange(fig_3, fig_4, ncol=2)

# humidity ratio
fig_5 <- 
  train_set %>%
  mutate(rnd_humid_r = round(humidity_ratio/5, digits=4)*5) %>%
  group_by(rnd_humid_r) %>%
  summarize(occupancy = mean(occupancy)) %>%
  ggplot(aes(rnd_humid_r, occupancy)) +
  geom_bar(stat='identity', color = 'black') +
  ggtitle("Figure 5: Occupancy Prevalence\nby Humidity Ratio") +
  xlab("Humidity Ratio (kg water_vapor/air)") +
  scale_y_continuous(name = "Occupancy", limits = c(0, 1)) +
  theme(plot.margin = unit(c(1,0.5,0,0.5),"cm"))

# date
fig_6 <- 
  train_set %>%
  mutate(date = date(date)) %>%
  group_by(date) %>%
  summarize(occupancy = mean(occupancy)) %>%
  ggplot(aes(date, occupancy)) +
  geom_bar(stat='identity', color = 'black') +
  ggtitle("Figure 6: Occupancy Prevalence\nby Date") +
  xlab("Date") +
  scale_y_continuous(name = "Occupancy", limits = c(0, 1)) +
  theme(plot.margin = unit(c(1,0.5,0,0.5),"cm"))

# display humidity ratio & date charts
grid.arrange(fig_5, fig_6, ncol=2)

# day of week
fig_7 <- 
  train_set %>%
  mutate(dow = wday(date, label = TRUE)) %>%
  group_by(dow) %>%
  summarize(occupancy = mean(occupancy), n=n()) %>%
  ggplot(aes(dow, occupancy)) +
  geom_bar(stat='identity', color = 'black') +
  ggtitle("Figure 7: Occupancy Prevalence\nby Day of Week") +
  xlab("Day of Week") +
  scale_y_continuous(name = "Occupancy", limits = c(0, 1)) +
  theme(plot.margin = unit(c(1,0.5,0,0.5),"cm"))

# hour of day
fig_8 <- 
  train_set %>%
  mutate(hour = hour(date)) %>%
  group_by(hour) %>%
  summarize(occupancy = mean(occupancy), n=n()) %>%
  ggplot(aes(hour, occupancy)) +
  geom_bar(stat='identity', color = 'black') +
  ggtitle("Figure 8: Occupancy Prevalence\nby Time of Day") +
  xlab("Hour") +
  scale_y_continuous(name = "Occupancy", limits = c(0, 1)) +
  theme(plot.margin = unit(c(1,0.5,0,0.5),"cm"))

# display day of week & hour of day charts
grid.arrange(fig_7, fig_8, ncol=2)

#### train models ####

## knn - data prep (store in new df)

# add day-of-week & hour-of-day
train_set_knn <-
  train_set %>%
  mutate(dow = wday(date, label = TRUE),
         hour = hour(date))

# combine weekend
levels(train_set_knn$dow) <- 
  c("Wknd", "Mon", "Tue", "Wed", "Thu", "Fri", "Wknd")
train_set_knn$dow <- as.character(train_set_knn$dow)

# combine late-night/early-morning hours
train_set_knn$hour <- 
  as.factor(ifelse(
    train_set_knn$hour <=6 | train_set_knn$hour >=19,
    0, train_set_knn$hour))

# remove original date field
train_set_knn <- within(train_set_knn, rm(date))

# dummy encoding pipeline
train_dummies <- dummyVars(~., train_set_knn)

# apply dummy encoding to train data
train_set_knn <- predict(train_dummies,
                         newdata = train_set_knn)

# k-fold crossvalidate & apply min-max scaler
control <- 
  trainControl(
    method="cv", 
    number=5,
    preProc = c("range") # min-max scaler
  )

# tune from k=3 to 23 (by 2)
grid <- expand.grid(k = seq(from=3, to=23, by=2))

## knn - train model
train_knn <- train(
  as.factor(occupancy) ~ .,
  method = "knn",
  tuneGrid=grid,
  trControl = control,
  data = train_set_knn)

# remove temp variables
rm(control, grid, train_set_knn)

# visualize crossvalidated results
train_knn$results %>%
  ggplot(aes(k, Accuracy)) +
  geom_point() +
  geom_line() +
  ggtitle("Figure 9: knn\nCrossvalidated Accuracy") +
  xlab("# of Neighbors") +
  ylab("Accuracy") +
  theme(plot.margin = unit(c(1,0.5,0,0.5),"cm"))


## gam - data prep (store in new df)

# round date to nearest hour
train_set_gam <-
  train_set %>%
  mutate(date = round_date(as_datetime(date), "hour"))

# k-fold crossvalidate
control <- trainControl(method="cv", number=5,)

# tune from span = 5% to 25% (by 5%). set degree = 1 (linear)
grid <- expand.grid(
  span = seq(from=0.05, to=.25, by=0.05),
  degree = 1
  )

## gam - train model
train_gam <- train(
  as.factor(occupancy) ~ .,
  method = "gamLoess",
  tuneGrid=grid,
  trControl = control,
  data = train_set_gam)

# remove temp variables
rm(control, grid, train_set_gam)

# visualize crossvalidated results
train_gam$results %>%
  ggplot(aes(span, Accuracy, color = factor(degree))) +
  geom_point() +
  geom_line() +
  ggtitle("Figure 10: GAM Loess\nCrossvalidated Accuracy") +
  xlab("span") +
  ylab("Accuracy") +
  labs(color = "Degree") +
  theme(plot.margin = unit(c(1,0.5,0,0.5),"cm"))

## random forest - data prep (store in new df)

# add day-of-week & hour-of-day
train_set_rf <- 
  train_set %>%
  mutate(dow = wday(date),
         hour = hour(date))

# remove original date field
train_set_rf <- within(train_set_rf, rm(date))


# k-fold crossvalidate
control <- trainControl(method="cv", number=5,)

# specify tune grid for crossvalidation
grid <- expand.grid(
  predFixed = seq(from=2, to=3, by=1),
  minNode = seq(from=10, to=100, by=30)
  )

## Random Forest - train model
train_rf <- 
  train(as.factor(occupancy) ~ ., 
        method = "Rborist", 
        tuneGrid = grid,
        trControl = control,
        data = train_set_rf
        )

# remove temp variables
rm(control, grid, train_set_rf)

# visualize crossvalidated results
train_rf$results %>%
  ggplot(aes(minNode, Accuracy, color = factor(predFixed))) +
  geom_point() +
  geom_line() +
  ggtitle("Figure 11: Random Forest\nCrossvalidated Accuracy") +
  xlab("Min Node Size") +
  ylab("Accuracy") +
  labs(color = "# of Randomly\nSelected\nPredictors") +
  theme(plot.margin = unit(c(1,0.5,0,0.5),"cm"))


#### predictions ####

## knn - replicate data prep

# add day-of-week and hour-of-day fields
test_set_knn <-
  test_set %>%
  mutate(dow = wday(date, label = TRUE),
         hour = hour(date))

# combine Sat, Sun into Weekend
levels(test_set_knn$dow) <- 
  c("Wknd", "Mon", "Tue", "Wed", "Thu", "Fri", "Wknd")
test_set_knn$dow <- as.character(test_set_knn$dow)

# combine late-night/early-morning hours
test_set_knn$hour <- as.factor(ifelse(test_set_knn$hour <=6 | test_set_knn$hour >=19, 0, test_set_knn$hour))

# remove original timestamp field
test_set_knn <- within(test_set_knn, rm(date))

# convert factors to dummy vars using fitted encoder
test_set_knn <- data.frame(
  predict(train_dummies, newdata = test_set_knn)
  )

## gam - replicate data prep

# round timestamp to the nearest hour
test_set_gam <-
  test_set %>%
  mutate(date = round_date(as_datetime(date), "hour"))

## Random Forest - replicate data prep

# add day-of-week and hour-of-day fields
test_set_rf <- 
  test_set %>%
  mutate(dow = wday(date),
         hour = hour(date))

# remove original timestamp field
test_set_rf <- within(test_set_rf, rm(date))


## make predictions

# make predictions using each model
pred_knn <- predict(train_knn, newdata=test_set_knn) # knn
pred_gam <- predict(train_gam, newdata=test_set_gam) # gam
pred_rf <- predict(train_rf, newdata=test_set_rf) # random forest

# remove temp objects
rm(train_dummies, test_set_knn, test_set_gam, test_set_rf)

# confusion matrix for each model (not used in rmd)
confusionMatrix(pred_knn, as.factor(test_set$occupancy), positive='1') # knn
confusionMatrix(pred_gam, as.factor(test_set$occupancy), positive='1') # gam
confusionMatrix(pred_rf, as.factor(test_set$occupancy), positive='1') # rf

#### ensemble ####

# combine model predictions into a dataframe
all_predictions <- data.frame(pred_rf, pred_gam, pred_knn)

# take the most common prediction
all_predictions['pred_ensemble'] <- 
  as.factor(ifelse(
    rowMeans(all_predictions == "1") > 0.5, "1", "0")
  )


#### model comparison ####

# calculate the accuracy of each model
accuracy <- 
  c(mean(all_predictions$pred_knn==as.factor(test_set$occupancy)),
    mean(all_predictions$pred_gam==as.factor(test_set$occupancy)),
    mean(all_predictions$pred_rf==as.factor(test_set$occupancy)),
    mean(all_predictions$pred_ensemble==as.factor(test_set$occupancy))
  )

# set column names and place in dataframe.
models <- c("K nearest neighbors", "Loess (GAM)", "Random Forest", "Ensemble")
model_accuarcies <- data.frame(Model = models, Accuracy = accuracy)

# nicely formatted table of model accuracies
kable(model_accuarcies)


# function to add line breaks to x-axis labels
add_line_break <- function(str, ...) {
  gsub('\\s', '\n', str)
}

# visualize final accuracies
model_accuarcies %>%
  ggplot(aes(x = models, y=accuracy)) +
  geom_point() +
  geom_text(label = label_percent(accuracy = 0.01)(accuracy), vjust = -1.5) +
  ggtitle("Figure 12: Final Accuracy, by Model") +
  scale_x_discrete(breaks=unique(models), labels=add_line_break(models)) +
  scale_y_continuous(limits = c(0.96, 1)) +
  theme(plot.margin = unit(c(1,0.5,0,0.5),"cm"))
