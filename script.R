# load required libraries
library(caret)
library(rpart)
library(tidyverse)
library(rpart.plot)
options("digits"=4)

cardioDf <- read.csv('cardio_train.csv', header = T, sep = ';')
str(cardioDf)

cardioDf$gender <- factor(cardioDf$gender, labels = c("female", "male"))

# Check for missing cases
cardioDf[!complete.cases(cardioDf),]

# Target classdistribution
table(cardioDf$cardio)

#Gender distributions
table(cardioDf$gender)

# Age distribution in the dataset
hist(round(cardioDf$age/365), main = "Age distribution", xlab = "Age(yrs)")


