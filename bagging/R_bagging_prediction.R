# R script for running bagging

require(rpart)
require(ipred)
set.seed(17)

# calibration step
calibrate <- read.table("calibration.txt", header=TRUE)
calibrate$classes <- as.factor(calibrate$classes)
calibrate.rf <- bagging(classes~band1+band2+band3+band4+band5+band7, data=calibrate, nbagg=120)

# prediction step
predict <- read.table("prediction.txt", header=TRUE)
memory.limit(size=4000)
predValues <- predict(calibrate.rf, newdata=predict)
predValues <- as.numeric(predValues)
write.table(predValues, "predValues.txt", row.names=FALSE, col.names="classes")
