library(neuralnet)  # Other packages include nnet , and RSNNS
library(readr)

dataset = read_csv("E:/Hofstra Assignements/Data Mining/Project1/dataset_tortuga.csv")

set.seed(123) # For reproducibility
dataset = as.data.frame(dataset[,-c(1,2)])
dataset$rndnum = runif(nrow(dataset), 1, 100)
dataset$PROFILE=as.numeric(factor(dataset$PROFILE))
# We can also use this 
apply(dataset,2,function(x) sum(is.na(x))) #data, margin - 2 is columns, function to be applied)

#min_max_normal <- function(x) {
 # return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
#}

# apply min max across all variables 
#dataset = as.data.frame(apply(dataset,2, FUN =min_max_normal))
# Replace NAs with the median in numeric columns
dataset[] <- lapply(dataset, function(col) {
  if (is.numeric(col)) {
    col[is.na(col)] <- median(col, na.rm = TRUE)
  }
  return(col)
})


# Assume dataset is already loaded
# Get the unique classes in PROFILE
unique_classes <- sort(unique(dataset$PROFILE))

# Manually create one-hot encoded columns
for (class in unique_classes) {
  # Create a new column for each class
  dataset[[paste0("Profile_", class)]] <- ifelse(dataset$PROFILE == class, 1, 0)
}

# Print the first few rows to verify
head(dataset)
dataset_train = dataset [ dataset[, "rndnum"] <= 80 , -c(1,15)]
dataset_test = dataset [ dataset[, "rndnum"] > 80 ,-c(1,15)]
sum(is.na(dataset_train))
# Number of inputs is number of columns minus one since the first row has the classification
inputs=ncol(dataset_train)
# Number of outputs is the number of possible classifiers
outputs = 1


neuron_estimate  = ceiling(nrow(dataset_train) / (2 * (inputs + outputs)))
n = colnames(dataset_train)   #note difference between colnames and names here, is based on return type

frm = 'Profile_1 + Profile_2 + Profile_3 + Profile_4 + Profile_5 + Profile_6 ~ '
for (i in 1:12) {
  
  frm = paste(frm, names(dataset_train)[i],sep="+")
  
}
frm



#frm = as.formula(paste("PROFILE ~", paste(n[!n %in% "PROFILE"], collapse = " + ")))
nn = neuralnet(frm,data=dataset_train,hidden=c(175,75),linear.output=F)

plot(nn)
print(nn)

dataset_test$predict = compute(nn, covariate = dataset_test[,1:12])$net.result

# since the prediction here is a decimal number we can make a cutoff point based on the original classificaiton 
#dataset_test$predict_class = ifelse(dataset_test$predict < .33, 1, ifelse( dataset_test$predict < .66, 2,3))

dataset_test$Predicted_Profile = apply(dataset_test[, paste0("Profile_", 1:6)], 1, function(row) {
  # Find the index of the maximum value in the row
  max_index <- which.max(row)
  return(max_index)  # Return the profile number corresponding to the max value
})
#dataset_test$Predicted_Profile = dataset$PROFILE  # put back the class as it originally was. 
#dataset_test$predict_win = as.numeric(dataset_test$predict_class == dataset_test$Predicted_Profile)

dataset_test$Correct_Prediction = ifelse(dataset_test$Predicted_Profile == dataset_test$PROFILE, 1, 0)

# Calculate accuracy as the percentage of correct predictions
accuracy = mean(dataset_test$Correct_Prediction) * 100
accuracy
# Print the accuracy
cat("Model Accuracy: ", accuracy, "%\n")

# Optionally, print a summary of correct and incorrect predictions
correct_count = sum(dataset_test$Correct_Prediction)
total_count = nrow(dataset_test)
cat("Correct Predictions: ", correct_count, "\n")
cat("Total Predictions: ", total_count, "\n")
