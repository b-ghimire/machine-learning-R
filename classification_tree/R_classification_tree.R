# R script for running classification tree model

require(rpart)
set.seed(17)

# calibration step
calibrate <- read.table("calibration.txt", header=TRUE)
calibrate$calibration <- as.factor(calibrate$classes)
calibrate.tree <- rpart(calibration~band1+band2+band3+band4+band5+band7+elevation+slope+aspect, method="class",data=calibrate, control=rpart.control(minsplit=4, cp=0.000001, usesurrogate=2))
printcp(calibrate.tree)
plotcp(calibrate.tree)

# pruning step
calibrate.tree.prune <- prune(calibrate.tree, cp=0.008)
plot(calibrate.tree.prune)
text(calibrate.tree.prune)

# prediction step
predict <- read.table("prediction.txt", header=TRUE)
memory.limit(size=4000)
predValues <- predict(calibrate.tree.prune, predict, type="class") 
predValues <- as.numeric(predValues)
write.table(predValues, "predValues.txt", row.names=FALSE, col.names="classes")
