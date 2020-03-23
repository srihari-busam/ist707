# load required libraries
library(caret)
library(rpart)
library(tidyverse)
library(rpart.plot)
library(arules)
library(arulesViz)

cardio_df <- read.csv('cardio_train.csv', header = T, sep = ';')
str(cardio_df)
nrow(cardio_df)

# convert age to years.
cardio_df$age <- round(cardio_df$age/365)

#Check Age range in data.
# Based on the data it seems the age ranges from 30-65 which seems reasonable.
range(cardio_df$age)

# Check height. Height is in cm
# keeping data for people only between 130cm(4.2ft) to 200cm(6.5ft). Removing remaing data.
range(cardio_df$height)
length(which(cardio_df$height > 200))
length(which(cardio_df$height < 130))
cardio_df <- cardio_df[-which(cardio_df$height < 130),]
cardio_df <- cardio_df[-which(cardio_df$height > 200),]

#check weight. Weight is in KG
# Removing the data assuming that weight below 40 and above 150kg are outliers.
range(cardio_df$weight)
length(which(cardio_df$weight < 40))
length(which(cardio_df$weight > 165))

cardio_df <- cardio_df[-which(cardio_df$weight < 40),]
cardio_df <- cardio_df[-which(cardio_df$weight > 165),]

# Check systolic blood pressure
# Based on this citation https://trialsjournal.biomedcentral.com/articles/10.1186/1468-6708-6-5 any value below 85 or above 200 is considered as outlier
range(cardio_df$ap_hi)
length(which(cardio_df$ap_hi < 85))
length(which(cardio_df$ap_hi > 240))
cardio_df <- cardio_df[-which(cardio_df$ap_hi < 85),]
cardio_df <- cardio_df[-which(cardio_df$ap_hi > 200),]

# Check systolic blood pressure
# Based on this citation https://trialsjournal.biomedcentral.com/articles/10.1186/1468-6708-6-5 any value below 85 or above 200 is considered as outlier
range(cardio_df$ ap_lo )
length(which(cardio_df$ ap_lo  < 50))
length(which(cardio_df$ ap_lo  > 140))
cardio_df <- cardio_df[-which(cardio_df$ ap_lo  < 50),]
cardio_df <- cardio_df[-which(cardio_df$ ap_lo  > 140),]

## Remove id
cardio_df <- cardio_df[ , !(names(cardio_df) %in% c("id"))]

str(cardio_df)
head(cardio_df)

# histograms to check gaussian distribution

layout(matrix(c(1,0,2,3,4,5), 3, 2, byrow = TRUE))
hist(cardio_df$age) + labs(title = "Age histogram")
hist(cardio_df$height)
hist(cardio_df$weight)
hist(cardio_df$ap_hi)
hist(cardio_df$ap_lo)
