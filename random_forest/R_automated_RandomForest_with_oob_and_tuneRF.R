# R script for running tuneRF function for random forest classification model
# This script runs the classifier for different combinations of the no of trees
# Date: 27 September, 2010 

# calling the random forest library
library(randomForest)

# reading input parameters text file
parameter1 <- read.table("param1.txt", header=TRUE)

# reading calibration data
calibrate <- read.table("calibration.txt", header=TRUE)

# converting to categorical values
calibrate$classes <- as.factor(calibrate$classes)

# counter stores the number of iterations
counter <- 1

# computing size of array to store results
no_elements1 <- as.integer(toString(parameter1$parameter1[1]))
no_elements <- no_elements1

# creates a matrix populated with 0's
no_trees <- matrix (0, no_elements)
no_split_var <- matrix(0, no_elements)
oob_error <- matrix(0,no_elements)

# adjusting values for loop
no_elements1 <- no_elements1+1

attach(calibrate)
pred_variables <- cbind(band1, band2, band3, band4)

# loops for varying input parameters
for (i in 2:no_elements1) {
    
  # tuning step
  tune.rf <- tuneRF(pred_variables, classes, ntreeTry=parameter1$parameter1[i], trace=FALSE, plot=FALSE, doBest=TRUE)
  
  # storing optimum number of split variables
  no_split_var[counter,1] <- tune.rf$mtry
  
  # storing the number of trees
  no_trees[counter,1] <- parameter1$parameter1[i]
  
  # running tuneRF again because this time doBest=FALSE which now allows to extract the oob error
  tune.rf <- tuneRF(pred_variables, classes, ntreeTry=parameter1$parameter1[i], trace=FALSE, plot=FALSE, doBest=FALSE)
  
  # finding dimesions
  dimension_tune.rf <- dim(tune.rf);
  
  # finding oob error
  for (j in 1:dimension_tune.rf[1]) {
    if (no_split_var[counter,1] == tune.rf[j,1]) {
      oob_error[counter,1] <- tune.rf[j,2]
    }      # end of if
  }  # end of for j loop
  
  # increasing the counter variable by 1
  counter <- counter + 1
  
}    # end of for i loop

# writing no of trees, no of split variables and out of bag (oob) error to text file
output_values <- paste(no_trees, "  ", no_split_var,"  ",oob_error)
write.table(output_values, "split_var_and_oob_error.txt", row.names=FALSE, col.names="no_of trees : no_split_variables : oob_error")
