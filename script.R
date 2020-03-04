# load required libraries
library(caret)
library(rpart)
library(tidyverse)
library(rpart.plot)
options("digits"=4)

cardioDf <- read.csv('cardio_train.csv', header = T, sep = ';')
str(cardioDf)
nrow(cardioDf)

cardioDf$age <- round(cardioDf$age/365)
cardioDf$gender <- factor(cardioDf$gender,
                          levels = c(1,2),
                          labels = c("female", "male"))
cardioDf$cholesterol <- factor(cardioDf$cholesterol,
                               levels = c(1,2,3),
                               labels = c("nomal", "aboveNormal", "high"),
                               ordered=T)
cardioDf$gluc <- factor(cardioDf$gluc,
                        levels = c(1,2,3),
                        labels = c("nomal", "aboveNormal", "high"),
                        ordered=T)
cardioDf$smoke <- factor(cardioDf$smoke,
                        levels = c(0,1),
                        labels = c("non-smoker", "smoker"))

cardioDf$active  <- factor(cardioDf$active,
                         levels = c(0,1),
                         labels = c("non-active", "active"))

cardioDf$alco  <- factor(cardioDf$alco,
                           levels = c(0,1),
                           labels = c("non-alcoholic", "alcoholic"))

cardioDf$cardio  <- factor(cardioDf$cardio,
                           levels = c(0,1),
                           labels = c("negative", "positive"))

hist(cardioDf$age)
hist(cardioDf$height)
summary(cardioDf$height)
sd(cardioDf$height)
which(cardioDf$height<140)
which(cardioDf$height>190)

hist(cardioDf$weight)
boxplot(cardioDf$weight)
which(cardioDf$weight<40)
which(cardioDf$weight>150)

which(cardioDf$ap_hi>190)
which(cardioDf$ap_hi<95)
boxplot(cardioDf$ap_hi)
hist(cardioDf$ap_hi)


unique(cardioDf$cholesterol)

# Check for missing cases
cardioDf[!complete.cases(cardioDf),]

# Target classdistribution
prop.table(table(cardioDf$cardio))

#Gender distributions
prop.table(table(cardioDf$gender))

# Age distribution in the dataset
hist(round(cardioDf$age/365), main = "Age distribution", xlab = "Age(yrs)")


