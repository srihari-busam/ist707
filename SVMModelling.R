# load required libraries
library(caret)
library(rpart)
library(tidyverse)
library(rpart.plot)
library(arules)
library(arulesViz)

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

## SVM Modelling
# https://medium.com/@alon.lek/should-i-look-at-precision-recall-or-specificity-sensitivity-3946158aace1
set.seed(123)
start_time <- Sys.time()
cardio_svm_linear <- train(cardio ~ ., data = train_df,
                                    method = "svmLinear",
                                    metric = "Recall",
                                    preProcess = c("center", "scale"),
                                    trControl = trainControl(method = "repeatedcv", number = 15, repeats = 3, allowParallel = T),
                                    tuneGrid = expand.grid(C = seq(0.1, 3.0, 0.2)))
end_time <- Sys.time()
end_time - start_time

# run on test data and calcualte metrics 
cardio_svm_linear_pred <- predict(cardio_svm_linear, newdata = train_df)
model_sentiment_svm_linear_pred_matrix <- confusionMatrix(cardio_svm_linear_pred, train_df$cardio)

# Get Accuracy, precision and Recall metrics
model_sentiment_svm_linear_pred_matrix$byClass


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

set.seed(123)
start_time <- Sys.time()
cardio_model_rf <- train(cardio ~ ., data = train_df,
                         method = "rf",
                         metric = "F1",
                         preProcess = c("center", "scale"),
                         trControl = trainControl(method = "repeatedcv", number = 15, repeats = 3, allowParallel = T))
end_time <- Sys.time()
end_time - start_time

set.seed(123)
start_time <- Sys.time()
model_knn <- train(cardio ~ ., data = train_df, method = "knn",
                   tuneGrid = data.frame(k = seq(20, 40)),
                   preProcess = c("center", "scale"),
                   trControl = trainControl(method = "repeatedcv", 
                                            number = 15, repeats = 3, allowParallel = T))
end_time <- Sys.time()
end_time - start_time
varImp(model_knn, scale=FALSE)

start_time <- Sys.time()
model_rpart <- train(cardio ~ ., data = train_df, method = "rpart",
                           trControl = trainControl(method = "repeatedcv", number = 15, repeats = 3, allowParallel = T),
                          tuneGrid = expand.grid(cp = seq(0, 0.5, 0.05))
                        )
end_time <- Sys.time()
end_time - start_time
varImp(model_rpart, scale=FALSE)

model_rpart_direct <- rpart(cardio ~ ., data = train_df, method = "class",  parms = list(split = "gini"), control  = rpart.control(minsplit = 10, minbucket = 5, cp = -1, xval=3))
model_rpart_direct_pred <- predict(model_rpart_direct, newdata = test_df, type = "class")
model_sentiment_svm_linear_pred_matrix <- confusionMatrix(model_rpart_direct_pred, test_df$cardio)


### NB
start_time <- Sys.time()
model_nb <- train(cardio ~ ., data = train_df, method = "nb",
                          preProcess = c("center", "scale"),
                          trControl = trainControl(method = "repeatedcv", number = 15,repeats=2, allowParallel = T),
                          tuneGrid = expand.grid(fL = 1:2, usekernel = c(TRUE, FALSE), adjust = 1:2))
end_time <- Sys.time()
end_time - start_time
