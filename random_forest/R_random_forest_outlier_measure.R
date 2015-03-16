# R script for calibrating random forest classification model and computing the outlier measure
# Make sure you set the best combination of parameters in the model before running the script 

library(randomForest)
set.seed(17)


# next section contains calibration step

calibrate <- read.table("calibration.txt", header=TRUE)
calibrate$classes <- as.factor(calibrate$classes)
calibrate.rf <- randomForest(classes~Band1+Band2+Band3+Band4+Band5+Band7, data=calibrate, mtry=2, importance=TRUE, proximity=TRUE)
print(calibrate.rf)


# next section contains outlier computation step

# outliers are computed for each pixel
outlier_measure <- outlier(calibrate.rf)

# finding absolute value of outlier_measure to remove negative sign
outlier_measure <- abs(outlier_measure)

# writing outlier measure for each pixel to text file
output_values <- paste(calibrate$classes," : ",outlier_measure)
write.table(output_values, "outlier_measure_per_pixel.txt", row.names=FALSE, col.names="classes : outlier measure")
