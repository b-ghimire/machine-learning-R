# R script for running boosting model
# This is an automated script and runs the classifier for different training/calibration data and parameter values
# Date: 19 May, 2010 

# kappa.r contains the function used in this script for computing kappa value
source("F:\\bagging_boosting_RF\\scripts\\R_kappa\\kappa.R")

# loading library
library(adabag)

# reading input parameters text file
parameter1 <- read.table("param1.txt", header=TRUE)
parameter2 <- read.table("param2.txt", header=TRUE)


# next section contains the calibration and prediction step

# reading calibration files     (any number of calibration files can be read)
cat('Input any number of calibration text file names and then press enter:', '\n')
calibration_file_name <- scan(what="character")

# reading validation file       (only one validation file can be read)
cat('Input only one validation text file name and then press enter:', '\n')
validation_file_name <- scan(what="character")

# finding the number of calibration files for the k loop
no_calibration_files <- length(calibration_file_name)

for (k in 1:no_calibration_files) {
  
  # reading the calibration data
  calibration_file_name_with_extension <- paste(calibration_file_name[k],".txt", sep="")
  calibration_file <- read.table(calibration_file_name_with_extension, header=TRUE)
  
  # reading the validation data
  validation_file_name_with_extension <- paste(validation_file_name[1],".txt", sep="")
  validation_file <- read.table(validation_file_name_with_extension, header=TRUE)
  
  # combining the calibration and validation files into a file called data_values
  data_values <- rbind (calibration_file, validation_file)
  
  # converting to factor for classification
  data_values$classes <- as.factor(data_values$classes)
  
  # finding the no of rows of data_values
  no_rows_data_values <- nrow(data_values)
  
  # finding the no of rows of calibration data
  no_rows_calibration <- nrow(calibration_file)
  
  # counter stores the number of iterations
  counter = 1
  
  # computing size of array to store results
  no_elements1 <- as.integer(toString(parameter1$parameter1[1]))
  no_elements2 <- as.integer(toString(parameter2$parameter2[1]))
  no_elements <- no_elements1*no_elements2
  
  # creates a matrix populated with 0's
  combination_name <- matrix(0,no_elements) 
  KappaValues <- matrix(0,no_elements)
  
  # adjusting values for loop
  no_elements1 <- no_elements1+1
  no_elements2 <- no_elements2+1
  
  # extract a subset from data which is used as calibration
  sub_train <- c(1:no_rows_calibration)
  
  # extract a subset from data which is used as validation
  sub_test <- c((no_rows_calibration+1):no_rows_data_values)
  
  # loops for varying input parameters
  for (i in 2:no_elements1) {
    for (j in 2:no_elements2)  {
      
      # concatenating the model parameters to prepare input to model
      model_input <- paste("classes","~",toString(parameter1$parameter1[i]))
      
      # converting the model_input variable from string to formula data type
      model_formula <- as.formula(model_input)
      
      # setting seed for the classfier so that the same results are generated when classification is rerun
      set.seed(17) 
      
      # calibration step
      calibrate.rf <- adaboost.M1(model_formula, data=data_values[sub_train,], mfinal=parameter2$parameter2[j])
      
      # prediction step for categories at locations of validation samples
      predValues <- predict.boosting(calibrate.rf, newdata=data_values[-sub_train,])
      
      # creating confusion matrix
      column_join <- cbind(as.data.frame(data_values$classes[sub_test]),predValues$class)
      confusion.matrix <- table(column_join)
      
      # function for computing kappa results
      kappa.result <- kappa(confusion.matrix)
      
      # storing the combination name as a string
      combination_name[counter,1] <- paste(parameter1$parameter1[i],"_",parameter2$parameter2[j],"  ")
      
      # storing Kappa values
      KappaValues[counter,1] <- kappa.result$sum.kappa
      
      # increasing the counter variable by 1
      counter <- counter + 1
    }   
  }  
  
  # writing kappa values to text file
  output_values <- paste(combination_name," : ",KappaValues)
  output_file_name <- paste(calibration_file_name[k], "_", "KappaValues.txt", sep="")
  write.table(output_values, output_file_name, row.names=FALSE, col.names="classes")
  
}  # end of k
