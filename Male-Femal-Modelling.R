# load required libraries
library(caret)
library(rpart)
library(tidyverse)
library(rpart.plot)
library(arules)
library(arulesViz)
library(rattle)

cardio_male_df <- read.csv('cardio_train.csv', header = T, sep = ';')
str(cardio_male_df)
nrow(cardio_male_df)

# convert age to years.
cardio_male_df$age <- round(cardio_male_df$age/365)

#Check Age range in data.
# Based on the data it seems the age ranges from 30-65 which seems reasonable.
range(cardio_male_df$age)

# Check height. Height is in cm
# keeping data for people only between 130cm(4.2ft) to 200cm(6.5ft). Removing remaing data.
range(cardio_male_df$height)
length(which(cardio_male_df$height > 200))
length(which(cardio_male_df$height < 130))
cardio_male_df <- cardio_male_df[-which(cardio_male_df$height < 130),]
cardio_male_df <- cardio_male_df[-which(cardio_male_df$height > 200),]

#check weight. Weight is in KG
# Removing the data assuming that weight below 40 and above 150kg are outliers.
range(cardio_male_df$weight)
length(which(cardio_male_df$weight < 40))
length(which(cardio_male_df$weight > 165))

cardio_male_df <- cardio_male_df[-which(cardio_male_df$weight < 40),]
cardio_male_df <- cardio_male_df[-which(cardio_male_df$weight > 165),]

# Check systolic blood pressure
# Based on this citation https://trialsjournal.biomedcentral.com/articles/10.1186/1468-6708-6-5 any value below 85 or above 200 is considered as outlier
range(cardio_male_df$ap_hi)
length(which(cardio_male_df$ap_hi < 85))
length(which(cardio_male_df$ap_hi > 240))
cardio_male_df <- cardio_male_df[-which(cardio_male_df$ap_hi < 85),]
cardio_male_df <- cardio_male_df[-which(cardio_male_df$ap_hi > 200),]

# Check systolic blood pressure
# Based on this citation https://trialsjournal.biomedcentral.com/articles/10.1186/1468-6708-6-5 any value below 85 or above 200 is considered as outlier
range(cardio_male_df$ ap_lo )
length(which(cardio_male_df$ ap_lo  < 50))
length(which(cardio_male_df$ ap_lo  > 140))
cardio_male_df <- cardio_male_df[-which(cardio_male_df$ ap_lo  < 50),]
cardio_male_df <- cardio_male_df[-which(cardio_male_df$ ap_lo  > 140),]

## Remove id
cardio_male_df <- cardio_male_df[ , !(names(cardio_male_df) %in% c("id"))]

str(cardio_male_df)
head(cardio_male_df)

#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
cardio_male_df$cardio <- factor(cardio_male_df$cardio, levels = c(0,1), labels = c("negative", "positive"))
nrow(cardio_male_df)

female_df = cardio_male_df[which(cardio_male_df$gender==1),]
male_df = cardio_male_df[which(cardio_male_df$gender==2),]
male_df = male_df[, -2]
female_df = female_df[,-2]
nrow(male_df)


set.seed(12345)
train_index <- createDataPartition(male_df$cardio, p = 0.80, list = FALSE)
train_male_df <- male_df[train_index,]
test_male_df <- male_df[-train_index,]
nrow(train_male_df)
nrow(test_male_df)

set.seed(123)
start_time <- Sys.time()
model_male_rpart <- train(cardio ~ ., data = train_male_df, method = "rpart",
                          trControl = trainControl(method = "repeatedcv", number = 15, repeats = 3, allowParallel = T),
                          tuneGrid = expand.grid(cp = seq(0, 0.5, 0.05))
)
end_time <- Sys.time()
end_time - start_time
varImp(model_male_rpart, scale=FALSE)
fancyRpartPlot(model_male_rpart$finalModel)


set.seed(123)
start_time <- Sys.time()
model_male_knn <- train(cardio ~ ., data = train_male_df, method = "knn",
                   tuneGrid = data.frame(k = seq(15, 30)),
                   preProcess = c("center", "scale"),
                   trControl = trainControl(method = "repeatedcv", 
                                            number = 15, repeats = 3, allowParallel = T))
end_time <- Sys.time()
end_time - start_time
varImp(model_male_knn, scale=FALSE)




### NB
set.seed(123)
start_time <- Sys.time()
model_male_nb <- train(cardio ~ ., data = train_male_df, method = "nb",
                  preProcess = c("center", "scale"),
                  trControl = trainControl(method = "repeatedcv", number = 15,repeats=3, allowParallel = T),
                  tuneGrid = expand.grid(fL = 1:2, usekernel = c(TRUE, FALSE), adjust = 1:2))
end_time <- Sys.time()
end_time - start_time

set.seed(123)
start_time <- Sys.time()
cardio_male_rf <- train(cardio ~ ., data = train_male_df,
                         method = "rf",
                         trControl = trainControl(method = "repeatedcv", number = 15, repeats = 3, allowParallel = T))
end_time <- Sys.time()
end_time - start_time

set.seed(123)
start_time <- Sys.time()
model_male_svm_linear <- train(cardio ~ ., data = train_male_df,
                           method = "svmLinear",
                           preProcess = c("center", "scale"),
                           trControl = trainControl(method = "repeatedcv", number = 15, repeats = 3, allowParallel = T),
                           tuneGrid = expand.grid(C = seq(0.1, 3.0, 0.5)))
end_time <- Sys.time()
end_time - start_time


set.seed(123)
start_time <- Sys.time()
model_male_svm_radial <- train(cardio ~ ., data = train_male_df,
                           tuneGrid = expand.grid(sigma = seq(0.1,0.2,0.1),
                                                  C = seq(0.1,1.0,0.5)),
                           method = "svmRadial",
                           preProcess = c("center", "scale"),
                           trControl = trainControl(method = "repeatedcv",
                                                    number = 15, repeats = 1,allowParallel = T))
end_time <- Sys.time()
end_time - start_time
varImp(model_male_svm_radial, scale=FALSE)


model_comparison <- resamples(list(RPart = model_rpart,RF = cardio_model_rf, NB = model_nb, KNN = model_knn, SVM_L = cardio_svm_linear), metric = "Recall")
scales <- list(x = list(relation = "free"),
               y = list(relation = "free"))

bwplot(model_comparison, scales = scales)
###############################################################################################
###############################################################################################
###### Female


set.seed(12345)
train_index_female <- createDataPartition(female_df$cardio, p = 0.80, list = FALSE)
train_female_df <- female_df[train_index_female,]
test_female_df <- female_df[-train_index_female,]
nrow(train_female_df)
nrow(test_female_df)

set.seed(123)
start_time <- Sys.time()
model_female_rpart <- train(cardio ~ ., data = train_female_df, method = "rpart",
                          trControl = trainControl(method = "repeatedcv", number = 15, repeats = 2, allowParallel = T),
                          tuneGrid = expand.grid(cp = seq(0, 0.5, 0.05))
)
end_time <- Sys.time()
end_time - start_time
varImp(model_female_rpart, scale=FALSE)
fancyRpartPlot(model_female_rpart$finalModel)


set.seed(123)
start_time <- Sys.time()
model_female_knn <- train(cardio ~ ., data = train_female_df, method = "knn",
                        tuneGrid = data.frame(k = seq(15, 30)),
                        preProcess = c("center", "scale"),
                        trControl = trainControl(method = "repeatedcv", 
                                                 number = 15, repeats = 2, allowParallel = T))
end_time <- Sys.time()
end_time - start_time
varImp(model_female_knn, scale=FALSE)




### NB
set.seed(123)
start_time <- Sys.time()
model_female_nb <- train(cardio ~ ., data = train_female_df, method = "nb",
                       preProcess = c("center", "scale"),
                       trControl = trainControl(method = "repeatedcv", number = 15,repeats=2, allowParallel = T),
                       tuneGrid = expand.grid(fL = 1:2, usekernel = c(TRUE, FALSE), adjust = 1:2))
end_time <- Sys.time()
end_time - start_time

set.seed(123)
start_time <- Sys.time()
model_female_rf <- train(cardio ~ ., data = train_female_df,
                        method = "rf",
                        trControl = trainControl(method = "repeatedcv", number = 15, repeats = 2, allowParallel = T))
end_time <- Sys.time()
end_time - start_time

set.seed(123)
start_time <- Sys.time()
model_female_svm_linear <- train(cardio ~ ., data = train_female_df,
                               method = "svmLinear",
                               preProcess = c("center", "scale"),
                               trControl = trainControl(method = "repeatedcv", number = 15, repeats = 2, allowParallel = T),
                               tuneGrid = expand.grid(C = seq(0.1, 3.0, 0.5)))
end_time <- Sys.time()
end_time - start_time


set.seed(123)
start_time <- Sys.time()
model_female_svm_radial <- train(cardio ~ ., data = train_female_df,
                               tuneGrid = expand.grid(sigma = seq(0.1,0.2,0.1),
                                                      C = seq(0.1,1.0,0.5)),
                               method = "svmRadial",
                               preProcess = c("center", "scale"),
                               trControl = trainControl(method = "repeatedcv",
                                                        number = 15, repeats = 2,allowParallel = T))

