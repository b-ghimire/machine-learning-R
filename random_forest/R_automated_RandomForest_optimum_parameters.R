# R script for running tuneRF function for random forest classification model
# tuneRF is used to find the optimum parameters ie. no of tree and no of split variables
# This script runs the classifier for different combinations of the no of trees
# Date: 27 September, 2010 

# kappa.r contains the function used in this script for computing kappa value
source("F:\\bagging_boosting_RF\\scripts\\R_kappa\\kappa.R")

# calls the random forest library
library(randomForest)

# calls the e1071 library
library (e1071)

# reading input parameters text file
parameter1 <- read.table("param1.txt", header=TRUE)
parameter2 <- read.table("param2.txt", header=TRUE)

# reading calibration data
calibrate <- read.table("calibration.txt", header=TRUE)

# converting to categorical values
calibrate$calibration <- as.factor(calibrate$calibration)

# reading validation data
validate <- read.table("validation.txt", header=TRUE)

# converting to categorical values
validate$validation <- as.factor(validate$validation)

# counter stores the number of iterations
counter <- 1

# computes size of array to store results
no_elements <- as.integer(toString(parameter1$parameter1[1]))

# creates a matrix populated with 0's
no_trees <- matrix (0, no_elements)
no_split_var <- matrix(0, no_elements)
KappaValues <- matrix(0, no_elements)

no_elements <- no_elements + 1

# loop for varying input parameters
for (i in 2:no_elements) {
  
  # concatenating the model parameters to prepare input to model
  model_input <- paste("calibration","~",toString(parameter1$parameter1[i]))
  
  # converting the model_input variable from string to formula data type
  model_formula <- as.formula(model_input)
  
  # tuning step
  tune.rf <- tune.randomForest(model_formula, data=calibrate, mtry=1:parameter2$parameter2[i], ntree=c(100,200,300,400))
  
  # storing optimum number of split variables
  no_split_var[counter,1] <- tune.rf$best.parameters$mtry
  
  # storing the optimum number of trees
  no_trees[counter,1] <- tune.rf$best.parameters$ntree
  
  # running random forest with the optimum input parameters
  calibrate.rf <- randomForest(model_formula, data=calibrate, mtry=tune.rf$best.parameters$mtry, ntree=tune.rf$best.parameters$ntree)
  
  # prediction step for categories at locations of validation samples
  predValues <- predict(calibrate.rf, validate)
  
  # creating confusion matrix
  column_join <- as.data.frame(cbind(validate$validation,predValues))
  confusion.matrix <- table(column_join)
  
  # function for computing kappa results
  kappa.result <- kappa(confusion.matrix)
   
  # storing Kappa values
  KappaValues[counter,1] <- kappa.result$sum.kappa
  
  # increasing the counter variable by 1
  counter <- counter + 1
  
}    # end of for i loop

# writing no of trees, no of split variables and out of bag (oob) error to text file
output_values <- paste(no_trees, "  ", no_split_var, "  ",KappaValues)
write.table(output_values, "optimum_parameters.txt", row.names=FALSE, col.names="optimum_no_of trees : optimum_no_split_variables : Kappa_values")
