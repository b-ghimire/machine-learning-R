# R script for calibrating and predicting using boosting

require(adabag)
set.seed(17)

# reading data
calibrate < -read.table("calibration_bands.txt", header=TRUE)
predict <- read.table("prediction_bands_part1.txt", header=TRUE)

# combining the calibration and validation files into a file called data_values
data_values <- rbind (calibrate, predict)

# finding the no of rows of calibration data
no_rows_calibration <- nrow(calibrate)

# extract subset from the data which is used as calibration
sub_train <- c(1:no_rows_calibration)

# converting to factor for classification
data_values$classes <- as.factor(data_values$classes)

# running boosting model
calibrate.rf <- adaboost.M1(classes~band1+band2+band3+band4+band5+band7, data=data_values[sub_train,], mfinal=500)

# prediction step
memory.limit(size=4000)
predValues <- predict.boosting(calibrate.rf, newdata=data_values[-sub_train,])
predValues <- as.numeric(predValues)
write.table(predValues, "predValues_part1.txt", row.names=FALSE, col.names="classes")
