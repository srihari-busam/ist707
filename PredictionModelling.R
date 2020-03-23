# load required libraries
library(caret)
library(rpart)
library(tidyverse)
library(rpart.plot)
library(arules)
library(arulesViz)
library(rattle)
library(gridExtra)
library(stargazer)


cardio_svm_df <- read.csv('cardio_train.csv', header = T, sep = ';')
str(cardio_svm_df)
nrow(cardio_svm_df)

# convert age to years.
cardio_svm_df$age <- round(cardio_svm_df$age/365)

#Check Age range in data.
# Based on the data it seems the age ranges from 30-65 which seems reasonable.
range(cardio_svm_df$age)

# Check height. Height is in cm
# keeping data for people only between 130cm(4.2ft) to 200cm(6.5ft). Removing remaing data.
range(cardio_svm_df$height)
length(which(cardio_svm_df$height > 200))
length(which(cardio_svm_df$height < 130))
cardio_svm_df <- cardio_svm_df[-which(cardio_svm_df$height < 130),]
cardio_svm_df <- cardio_svm_df[-which(cardio_svm_df$height > 200),]

#check weight. Weight is in KG
# Removing the data assuming that weight below 40 and above 150kg are outliers.
range(cardio_svm_df$weight)
length(which(cardio_svm_df$weight < 40))
length(which(cardio_svm_df$weight > 165))

cardio_svm_df <- cardio_svm_df[-which(cardio_svm_df$weight < 40),]
cardio_svm_df <- cardio_svm_df[-which(cardio_svm_df$weight > 165),]

# Check systolic blood pressure
# Based on this citation https://trialsjournal.biomedcentral.com/articles/10.1186/1468-6708-6-5 any value below 85 or above 200 is considered as outlier
range(cardio_svm_df$ap_hi)
length(which(cardio_svm_df$ap_hi < 85))
length(which(cardio_svm_df$ap_hi > 240))
cardio_svm_df <- cardio_svm_df[-which(cardio_svm_df$ap_hi < 85),]
cardio_svm_df <- cardio_svm_df[-which(cardio_svm_df$ap_hi > 200),]

# Check systolic blood pressure
# Based on this citation https://trialsjournal.biomedcentral.com/articles/10.1186/1468-6708-6-5 any value below 85 or above 200 is considered as outlier
range(cardio_svm_df$ ap_lo )
length(which(cardio_svm_df$ ap_lo  < 50))
length(which(cardio_svm_df$ ap_lo  > 140))
cardio_svm_df <- cardio_svm_df[-which(cardio_svm_df$ ap_lo  < 50),]
cardio_svm_df <- cardio_svm_df[-which(cardio_svm_df$ ap_lo  > 140),]

## Remove id
cardio_svm_df <- cardio_svm_df[ , !(names(cardio_svm_df) %in% c("id"))]

str(cardio_svm_df)
head(cardio_svm_df)
70000 -nrow(cardio_svm_df)

#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
cardio_svm_df$cardio <- factor(cardio_svm_df$cardio, levels = c(0,1), labels = c("negative", "positive"))

set.seed(12345)
train_index <- createDataPartition(cardio_svm_df$cardio, p = 0.80, list = FALSE)
train_df <- cardio_svm_df[train_index,]
test_df <- cardio_svm_df[-train_index,]
nrow(train_df)
nrow(test_df)
nrow(train_df)
nrow(test_df)

## Random forest
set.seed(123)
start_time <- Sys.time()
cardio_model_rf <- train(cardio ~ ., data = train_df,
                         method = "rf",
                         metric = "F1",
                         trControl = trainControl(method = "repeatedcv", number = 15, repeats = 3, allowParallel = T))
end_time <- Sys.time()
end_time - start_time

## SVM Linear
# https://medium.com/@alon.lek/should-i-look-at-precision-recall-or-specificity-sensitivity-3946158aace1
set.seed(123)
start_time <- Sys.time()
cardio_svm_linear <- train(cardio ~ ., data = train_df,
                                    method = "svmLinear",
                                    preProcess = c("center", "scale"),
                                    trControl = trainControl(method = "repeatedcv", number = 15, repeats = 3, allowParallel = T),
                                    tuneGrid = expand.grid(C = seq(0.1, 3.0, 0.5)))
end_time <- Sys.time()
end_time - start_time


## SVM Radial
set.seed(123)
start_time <- Sys.time()
cardio_svm_radial <- train(cardio ~ ., data = train_df,
                       tuneGrid = expand.grid(sigma = seq(0.1,0.3,0.1),
                                              C = seq(0.1,0.3,0.1)),
                       method = "svmRadial",
                       preProcess = c("center", "scale"),
                       trControl = trainControl(method = "repeatedcv",
                                                number = 15, repeats = 2,allowParallel = T))
end_time <- Sys.time()
end_time - start_time
varImp(cardio_svm_radial, scale=FALSE)


## KNN
set.seed(123)
start_time <- Sys.time()
model_knn <- train(cardio ~ ., data = train_df, method = "knn",
                   tuneGrid = data.frame(k = seq(1, 25)),
                   preProcess = c("center", "scale"),
                   trControl = trainControl(method = "repeatedcv", 
                                            number = 15, repeats = 3, allowParallel = T))
end_time <- Sys.time()
end_time - start_time
varImp(model_knn, scale=FALSE)

## RPART
start_time <- Sys.time()
model_rpart <- train(cardio ~ ., data = train_df, method = "rpart",
                           trControl = trainControl(method = "repeatedcv", number = 15, repeats = 3, allowParallel = T),
                          tuneGrid = expand.grid(cp = seq(0, 0.5, 0.05))
                        )
end_time <- Sys.time()
end_time - start_time


### NB
start_time <- Sys.time()
model_nb <- train(cardio ~ ., data = train_df, method = "nb",
                          preProcess = c("center", "scale"),
                          trControl = trainControl(method = "repeatedcv", number = 15,repeats=3, allowParallel = T),
                          tuneGrid = expand.grid(fL = 1:2, usekernel = c(TRUE, FALSE), adjust = 1:2))
end_time <- Sys.time()
end_time - start_time

## Model comparision
model_comparison <- resamples(list(DecisionTree = model_rpart,
                                   RandomForest = cardio_model_rf, 
                                   NaiveBayes = model_nb, 
                                   KNN = model_knn, 
                                   SVMLinear = cardio_svm_linear), metric = "Recall")
scales <- list(x = list(relation = "free"),
               y = list(relation = "free"))

bwplot(model_comparison, scales = scales, main="Classification modelling comparision")


## Aux functiona final reports
modelAnalysis <- function(model, varImpTitle){
  
  print(model$bestTune)
  print(knitr::kable(model$results, digits = 2, caption = "TEST."))
  print(model$finalModel)
  
  pred_value <- predict(model, newdata = test_df)
  conf_matrix <- confusionMatrix(pred_value, test_df$cardio)
  print(conf_matrix$table)
  print(conf_matrix$byClass)
  
  plot(varImp(model), main = varImpTitle)
  
}

## DT
modelAnalysis(model_rpart, "Variable Importance with Decision Tree")
fancyRpartPlot(model_rpart$finalModel, main = "Decision Tree")

## KNN
modelAnalysis(model_knn, "Variable Importance with KNN")
knitr::kable(model_knn$results, digits = 2, caption = "TEST.")
plot(model_knn, main="KNN Accuracy with neighbours")

## NB
modelAnalysis(model_nb, "Variable Importance with Naive Bayes")
knitr::kable(model_nb$results, digits = 2, caption = "TEST.")
plot(model_nb, main="Naive Bayes accuracy")
model_nb$finalModel

## SVM LINEAR
modelAnalysis(cardio_svm_linear, "Variable Importance with SVM-Linear")
knitr::kable(cardio_svm_linear$results, digits = 2, caption = "TEST.")
plot(cardio_svm_linear, main="SVM-Linear accuracy")
cardio_svm_linear$finalModel

## SVM RAIDAL

modelAnalysis(cardio_svm_radial, "Variable Importance with SVM-Radial")
knitr::kable(cardio_svm_radial$results, digits = 2, caption = "TEST.")
plot(cardio_svm_radial, main="SVM-Radial accuracy")
cardio_svm_radial$finalModel


model_rpart$bestTune
model_rpart$results
model_rpart$finalModel

pred_value <- predict(model_rpart, newdata = test_df)
conf_matrix <- confusionMatrix(pred_value, test_df$cardio)
conf_matrix$table
conf_matrix$byClass

print(conf_matrix)
plot(varImp(model), main = varImpTitle)

options("digits"=4)
grid.table(model_rpart$results)
knitr::kable(model_rpart$results, digits = 2, caption = "TEST.")
print.data.frame(model_rpart$results, digits =2)
