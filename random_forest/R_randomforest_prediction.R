# R script for calibrating and predicting using random forest model

library(randomForest)
set.seed(17)

# calibration step
calibrate <- read.table("calibration.txt", header=TRUE)
calibrate$classes <- as.factor(calibrate$classes)
calibrate.rf <- randomForest(classes~band1+band2+band3+band4+band5+band7, data=calibrate, mtry=6, ntree=500, importance=TRUE, proximity=TRUE)

# prediction step
predict <- read.table("prediction.txt", header=TRUE)
memory.limit(size=4000)
predValues <- predict(calibrate.rf, predict)
predValues <- as.numeric(predValues)
write.table(predValues, "predValues.txt", row.names=FALSE, col.names="classes")
