# load required libraries
library(caret)
library(rpart)
library(tidyverse)
library(rpart.plot)
library(arules)
library(arulesViz)
options("digits"=4)

cardioARulesDf <- read.csv('cardio_train.csv', header = T, sep = ';')
str(cardioARulesDf)
nrow(cardioARulesDf)

# convert age to years.
cardioARulesDf$age <- round(cardioARulesDf$age/365)

#Check Age range in data.
# Based on the data it seems the age ranges from 30-65 which seems reasonable.
range(cardioARulesDf$age)

# Check height. Height is in cm
# keeping data for people only between 130cm(4.2ft) to 200cm(6.5ft). Removing remaing data.
range(cardioARulesDf$height)
length(which(cardioARulesDf$height > 200))
length(which(cardioARulesDf$height < 130))
cardioARulesDf <- cardioARulesDf[-which(cardioARulesDf$height < 130),]
cardioARulesDf <- cardioARulesDf[-which(cardioARulesDf$height > 200),]

#check weight. Weight is in KG
# Removing the data assuming that weight below 40 and above 150kg are outliers.
range(cardioARulesDf$weight)
length(which(cardioARulesDf$weight < 40))
length(which(cardioARulesDf$weight > 165))

cardioARulesDf <- cardioARulesDf[-which(cardioARulesDf$weight < 40),]
cardioARulesDf <- cardioARulesDf[-which(cardioARulesDf$weight > 165),]

# Check systolic blood pressure
# Based on this citation https://trialsjournal.biomedcentral.com/articles/10.1186/1468-6708-6-5 any value below 85 or above 200 is considered as outlier
range(cardioARulesDf$ap_hi)
length(which(cardioARulesDf$ap_hi < 85))
length(which(cardioARulesDf$ap_hi > 240))
cardioARulesDf <- cardioARulesDf[-which(cardioARulesDf$ap_hi < 85),]
cardioARulesDf <- cardioARulesDf[-which(cardioARulesDf$ap_hi > 200),]

# Check systolic blood pressure
# Based on this citation https://trialsjournal.biomedcentral.com/articles/10.1186/1468-6708-6-5 any value below 85 or above 200 is considered as outlier
range(cardioARulesDf$ ap_lo )
length(which(cardioARulesDf$ ap_lo  < 50))
length(which(cardioARulesDf$ ap_lo  > 140))
cardioARulesDf <- cardioARulesDf[-which(cardioARulesDf$ ap_lo  < 50),]
cardioARulesDf <- cardioARulesDf[-which(cardioARulesDf$ ap_lo  > 140),]

## Remove id
cardioARulesDf <- cardioARulesDf[ , !(names(cardioARulesDf) %in% c("id"))]

## Convert to factors
cardioARulesDf$gender <- factor(cardioARulesDf$gender,
                          levels = c(1,2),
                          labels = c("female", "male"))
cardioARulesDf$cholesterol <- factor(cardioARulesDf$cholesterol,
                               levels = c(1,2,3),
                               labels = c("nomal", "aboveNormal", "high"),
                               ordered=T)
cardioARulesDf$gluc <- factor(cardioARulesDf$gluc,
                        levels = c(1,2,3),
                        labels = c("nomal", "aboveNormal", "high"),
                        ordered=T)
cardioARulesDf$smoke <- factor(cardioARulesDf$smoke,
                         levels = c(0,1),
                         labels = c("non-smoker", "smoker"))

cardioARulesDf$active  <- factor(cardioARulesDf$active,
                           levels = c(0,1),
                           labels = c("non-active", "active"))

cardioARulesDf$alco  <- factor(cardioARulesDf$alco,
                         levels = c(0,1),
                         labels = c("non-alcoholic", "alcoholic"))

cardioARulesDf$cardio  <- factor(cardioARulesDf$cardio,
                           levels = c(0,1),
                           labels = c("negative", "positive"))

cardioARulesDf <- cardioARulesDf %>% mutate(age = case_when(age < 40 ~ '30s',
                                             age >= 40  & age < 50 ~ '40s',
                                             age >= 50  & age <= 60 ~ '50s',
                                             age >= 60 ~ 'Above60s'))
cardioARulesDf$age <- factor(cardioARulesDf$age)

cardioARulesDf$height <- discretize(cardioARulesDf$height, method = "interval", breaks = 3, labels=c("short","medium","tall"))
cardioARulesDf$weight <- discretize(cardioARulesDf$weight, method = "interval", breaks = 3, labels=c("light","medium","heavy"))

cardioARulesDf$ap_hi <- discretize(cardioARulesDf$ap_hi, method = "fixed", breaks = c(-Inf, 120, 130, 140, 180, Inf), 
           labels = c("normal", "elevated", "high1", "high2", "hypertesive"))
cardioARulesDf$ap_lo <- discretize(cardioARulesDf$ap_lo, method = "fixed", breaks = c(-Inf, 80, 90,120, Inf), 
                                   labels = c("normal", "high1", "high2", "hypertesive"))


str(cardioARulesDf)

associationrules <- apriori(cardioARulesDf, parameter = list(support = 0.2, confidence = 0.5, minlen = 8))
rules<-sort(associationrules, decreasing=TRUE,by='confidence')
inspect(head(sort(associationrules, by = "lift", decreasing = T), 10))

## Positive prediction rules
cardioPositiveRules <- apriori(data = cardioARulesDf, parameter = list(supp = 0.1, conf = 0.5, minlen = 6),
                 appearance = list(default="lhs",rhs="cardio=positive"),
                 control = list(verbose = F))
cardioPositiveRules<-sort(cardioPositiveRules, decreasing=TRUE,by='confidence')
inspect(head(sort(cardioPositiveRules, by = "lift", decreasing = T), 10))
plot(cardioPositiveRules, measure = c("support", "lift"), shading = "confidence")

## Negative prediction rules
cardioNegativeRules <- apriori(data = cardioARulesDf, parameter = list(supp = 0.1, conf = 0.5, minlen =7),
                               appearance = list(default="lhs",rhs="cardio=negative"),
                               control = list(verbose = F))
cardioNegativeRules<-sort(cardioNegativeRules, decreasing=TRUE,by='confidence')
inspect(head(sort(cardioNegativeRules, by = "lift", decreasing = T), 10))
plot(cardioNegativeRules, measure = c("support", "lift"), shading = "confidence")
