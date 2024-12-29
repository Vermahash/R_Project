dataset = read_csv("E:/Hofstra Assignements/Data Mining/Project1/dataset_tortuga.csv")
dataset = as.data.frame
dataset
dataset$PROFILE = as.factor(dataset$PROFILE)
summary(dataset)
# Assuming your dataset is named `df` and the column to be evaluated is `skill_level`
dataset$refined_profile = ifelse(
  dataset$PROFILE %in% c("advanced_backend", "advanced_data_science", "advanced_front_end"), 
  1, 
  ifelse(dataset$PROFILE %in% c("beginner_backend", "beginner_data_science", "beginner_front_end"), 
         0, 
         NA)
)
View(dataset)
dataset = as.data.frame(dataset[,-c(1,2,16)])
colnames(dataset)
dataset$rndnum = runif(nrow(dataset), 1, 100)
dataset_train = dataset [ dataset[, "rndnum"] <= 80 , ]
dataset_test = dataset [ dataset[, "rndnum"] > 80 , ]

dataset_train <- na.omit(dataset_train)


mdl = lm(refined_profile ~ USER_ID + HOURS_DATASCIENCE + HOURS_BACKEND  + HOURS_FRONTEND  + NUM_COURSES_BEGINNER_DATASCIENCE +NUM_COURSES_BEGINNER_BACKEND + NUM_COURSES_BEGINNER_FRONTEND + NUM_COURSES_ADVANCED_DATASCIENCE + NUM_COURSES_ADVANCED_BACKEND + NUM_COURSES_ADVANCED_FRONTEND + AVG_SCORE_DATASCIENCE + AVG_SCORE_BACKEND + AVG_SCORE_FRONTEND ,  data = dataset_train)
summary(mdl)
mdl = lm(refined_profile ~  HOURS_DATASCIENCE + HOURS_BACKEND  + NUM_COURSES_BEGINNER_DATASCIENCE +NUM_COURSES_BEGINNER_BACKEND + NUM_COURSES_BEGINNER_FRONTEND + NUM_COURSES_ADVANCED_DATASCIENCE + NUM_COURSES_ADVANCED_BACKEND + NUM_COURSES_ADVANCED_FRONTEND + AVG_SCORE_DATASCIENCE + AVG_SCORE_BACKEND + AVG_SCORE_FRONTEND ,  data = dataset_train)
summary(mdl)
mdl = lm(refined_profile ~ HOURS_DATASCIENCE + HOURS_BACKEND  + NUM_COURSES_BEGINNER_DATASCIENCE +NUM_COURSES_BEGINNER_BACKEND + NUM_COURSES_BEGINNER_FRONTEND + NUM_COURSES_ADVANCED_DATASCIENCE + NUM_COURSES_ADVANCED_BACKEND + NUM_COURSES_ADVANCED_FRONTEND + AVG_SCORE_DATASCIENCE + AVG_SCORE_BACKEND + AVG_SCORE_FRONTEND ,  data = dataset_train)
summary(mdl)
dataset_train$err=mdl$residuals
dataset_train$pcterror=dataset_train$err/dataset_train$refined_profile
dataset_train$abserr=abs(dataset_train$err)
dataset_train$abspcterr=abs(dataset_train$pcterr)
dataset_train$sqerr=(dataset_train$err)^2
mean(dataset_train$err)
mean(dataset_train$pcterror)
mean(dataset_train$abserr)
mean(dataset_train$abspcterr)
sqrt(mean(dataset_train$sqerr))

dataset_test <- na.omit(dataset_test)
dataset_test$pred=predict(mdl,dataset_test)
dataset_test$pred
dataset_test$rounded_predictions <- ifelse(
  dataset_test$pred < 0.5, 0, 
  ifelse(dataset_test$pred > 0.5, 1, dataset_test$pred)
)

dataset_test$err=dataset_test$refined_profile-dataset_test$rounded_predictions
dataset_test$pcterror=dataset_test$err/dataset_test$refined_profile
dataset_test$abserr=abs(dataset_test$err)
dataset_test$abspcterr=abs(dataset_test$pcterr)
dataset_test$sqerr=(dataset_test$err)^2


mean(dataset_test$err)
mean(dataset_test$pcterror)
mean(dataset_test$abserr)
mean(dataset_test$abspcterr)
sqrt(mean(dataset_test$sqerr))
View(dataset_train)
View(dataset_test)

accuracy = sum(dataset_test$refined_profile == dataset_test$rounded_predictions) / nrow(dataset_test)
cat("Model Accuracy:", accuracy * 100, "%\n")



# Load necessary libraries

