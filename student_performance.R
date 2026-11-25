
# Student Performance Prediction Using Naive Bayes in R

# 1. Install and load required package
if (!require(e1071)) {
  install.packages("e1071")
}

library(e1071)

# 2. Create a sample student dataset
students <- data.frame(
  Study_Hours = c(2, 3, 5, 6, 1, 4, 7, 2, 8, 5,
                  3, 6, 1, 7, 4, 8, 2, 5, 6, 3),
  
  Attendance = c(50, 60, 85, 90, 40, 75, 95, 55, 98, 80,
                 65, 88, 35, 92, 70, 96, 45, 82, 87, 62),
  
  Previous_Score = c(40, 45, 75, 80, 30, 65, 90, 42, 95, 72,
                     50, 85, 25, 88, 68, 96, 38, 78, 83, 48),
  
  Result = factor(c(
    "Fail", "Fail", "Pass", "Pass", "Fail",
    "Pass", "Pass", "Fail", "Pass", "Pass",
    "Fail", "Pass", "Fail", "Pass", "Pass",
    "Pass", "Fail", "Pass", "Pass", "Fail"
  ))
)

# 3. Display the dataset
print(students)

# 4. Split data into training and testing sets
set.seed(42)

train_index <- sample(
  1:nrow(students),
  size = 0.75 * nrow(students)
)

train_data <- students[train_index, ]
test_data <- students[-train_index, ]

# 5. Train the Naive Bayes model
model <- naiveBayes(
  Result ~ Study_Hours + Attendance + Previous_Score,
  data = train_data
)

print(model)

# 6. Predict student performance
predictions <- predict(model, test_data)

# 7. Evaluate model performance
conf_matrix <- table(
  Actual = test_data$Result,
  Predicted = predictions
)

print("Confusion Matrix:")
print(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

# 8. Predict result for a new student
new_student <- data.frame(
  Study_Hours = 5,
  Attendance = 85,
  Previous_Score = 72
)

new_prediction <- predict(model, new_student)

print("New Student Prediction:")
print(new_prediction)
