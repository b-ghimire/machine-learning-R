# This is a script for determining the McNemar test for comparing two random forest classifications
# referece: Foody GM. 2009. Sample size determination for image classification accuracy assessment and comparison. IJRS 30(20):5273-5291
# Date: 2 November, 2010 

# calling the random forest library
library(randomForest)

# next section contains the calibration and prediction steps

# reading calibration data
calibrate <- read.table("calibration.txt", header=TRUE)

# converting to categorical values
calibrate$calibration <- as.factor(calibrate$calibration)

# reading validation data
validate <- read.table("validation.txt", header=TRUE)

# converting to categorical values
validate$validation <- as.factor(validate$validation)

# setting seed for the classfier so that the same results are generated when classification is rerun
set.seed(17)

# calibration step for first model
calibrate.rf.model1 <- randomForest(calibration~band1+band2+band3+band4, data=calibrate, mtry=3, ntree=100)

# calibration step for second model
calibrate.rf.model2 <- randomForest(calibration~band1+band2+band3, data=calibrate, mtry=2, ntree=50)

# prediction step for categories at locations of validation samples for model 1
predValues.model1 <- predict(calibrate.rf.model1, validate)

# prediction step for categories at locations of validation samples for model 2
predValues.model2 <- predict(calibrate.rf.model2, validate)

# storing the validation samples into an array
validation_sample <- validate$validation

# determining the number of elements in a vector
no_elements <- length(predValues.model1)

# intializing the values of the counters
count_correct1_correct2 <- 0
count_incorrect1_correct2 <- 0
count_correct1_incorrect2 <- 0
count_incorrect1_incorrect2 <- 0

for (i in 1:no_elements) {
  
  if ((predValues.model1[i]==validation_sample[i]) && (predValues.model2[i]==validation_sample[i]))
    count_correct1_correct2 = count_correct1_correct2+1   # correct for model 1 and correct for model 2
  
  if ((predValues.model1[i]!=validation_sample[i]) && (predValues.model2[i]==validation_sample[i]))
    count_incorrect1_correct2 = count_incorrect1_correct2+1     # incorrect for model 1 and correct for model 2
  
  if ((predValues.model1[i]==validation_sample[i]) && (predValues.model2[i]!=validation_sample[i]))
    count_correct1_incorrect2 = count_correct1_incorrect2+1     # correct for model 1 and incorrect for model 2
  
  if ((predValues.model1[i]!=validation_sample[i]) && (predValues.model2[i]!=validation_sample[i]))
    count_incorrect1_incorrect2 = count_incorrect1_incorrect2+1   # incorrect for model 1 and incorrect for model 2
  
}

# creating a 2 by 2 matrix of the counters
x <- matrix(c(count_correct1_correct2, count_incorrect1_correct2, count_correct1_incorrect2,count_incorrect1_incorrect2), nrow = 2, ncol = 2, byrow = TRUE)

# running the McNemar test
mcnemar.test(x)
