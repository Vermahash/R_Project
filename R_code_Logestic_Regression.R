library(caret) # For splitting the dataset
library(ggplot2) # Optional for visualizations
#Logestic Regression Using GPT but knowing what code is doing.
# Load the dataset
dataset = read_csv("E:/Hofstra Assignements/Data Mining/Project1/dataset_tortuga.csv")

# Convert the response variable to a factor (binary classification)
dataset$PROFILE = as.factor(dataset$PROFILE)
dataset$refined_profile = ifelse(
  dataset$PROFILE %in% c("advanced_backend", "advanced_data_science", "advanced_front_end"), 
  1, 
  ifelse(dataset$PROFILE %in% c("beginner_backend", "beginner_data_science", "beginner_front_end"), 
         0, 
         NA)
)
# Split the data into training and testing sets
set.seed(123) # For reproducibility
dataset = as.data.frame(dataset[,-c(1,2,16)])
dataset$rndnum = runif(nrow(dataset), 1, 100)
dataset_train = dataset [ dataset[, "rndnum"] <= 80 , ]
dataset_test = dataset [ dataset[, "rndnum"] > 80 , ]


# Fit logistic regression model
mdl = glm(refined_profile ~  HOURS_DATASCIENCE + HOURS_BACKEND  + HOURS_FRONTEND  + NUM_COURSES_BEGINNER_DATASCIENCE +NUM_COURSES_BEGINNER_BACKEND + NUM_COURSES_BEGINNER_FRONTEND + NUM_COURSES_ADVANCED_DATASCIENCE + NUM_COURSES_ADVANCED_BACKEND + NUM_COURSES_ADVANCED_FRONTEND + AVG_SCORE_DATASCIENCE + AVG_SCORE_BACKEND + AVG_SCORE_FRONTEND ,  data = dataset_train, family = binomial)
summary(mdl)
mdl = lm(refined_profile ~  HOURS_DATASCIENCE + HOURS_BACKEND  + NUM_COURSES_BEGINNER_DATASCIENCE +NUM_COURSES_BEGINNER_BACKEND + NUM_COURSES_BEGINNER_FRONTEND + NUM_COURSES_ADVANCED_DATASCIENCE + NUM_COURSES_ADVANCED_BACKEND + NUM_COURSES_ADVANCED_FRONTEND + AVG_SCORE_DATASCIENCE + AVG_SCORE_BACKEND + AVG_SCORE_FRONTEND ,  data = dataset_train)
summary(mdl)

# Predict on the test data
predicted_prob = predict(mdl, newdata = dataset_test, type = "response")
dataset_test$pred=predict(mdl,dataset_test,type = "response")
dataset_test$rounded_predictions = ifelse(
  dataset_test$pred < 0.5, 0, 
  ifelse(dataset_test$pred > 0.5, 1, dataset_test$pred)
)
# Evaluate model performance
confusion_matrix = table(Predicted = dataset_test$rounded_predictions, Actual = dataset_test$refined_profile)
print("Confusion Matrix:")
print(confusion_matrix)

# Calculate accuracy
accuracy = sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))