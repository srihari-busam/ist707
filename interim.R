library(caret)
library(rpart)
library(tidyverse)
#library(rpart.plot)
#library(arules)
library(stats)
library(cluster)
library(dplyr)
library(ggvis)
library(tidyr)
library(Rtsne)
options("digits"=4)

cardioARulesDf <- read.csv("cardio_train.csv", header = T, sep = ';')
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


str(cardioARulesDf)
scaled_cadio_df <- scale(cardioARulesDf, center=T, scale =T)
summary(scaled_cadio_df)
#Distance fucniton and visualization
#Kmeans clustering

#Get the values for WCSS within groups sum of squares
getWCSSData <- function(X) {
  # args:
  # X: the prepared data for clustering
  # Output:
  # List with the WCSS values for cluster from 1 to length of the list
  #set seed reproducible random number so the centroid would start from the same space
  set.seed(13)
  wcss_values <- vector()
  max_wcss_steps = sqrt(length(X[,1]))
  for(i in 1:20) {
    wcss_values[i] <- sum(kmeans(X, i, nstart = 250, iter.max = 1000, algorithm="Lloyd")$withinss)
  }
  return(wcss_values)
}

#use elbow function to determine appropriate number of clusters
#function to get elbow points
getElbowPoint <- function(x_values, y_values){
  # Max values to create line
  max_x_x <- max(x_values)
  max_x_y <- y_values[which.max(x_values)]
  max_y_y <- max(y_values)
  max_y_x <- x_values[which.max(y_values)]
  max_df <- data.frame(x = c(max_y_x, max_x_x), y = c(max_y_y, max_x_y))
  
  # Creating straight line between the max values
  fit <- lm(max_df$y ~ max_df$x)
  
  # Distance from point to line
  distances <- c()
  for(i in 1:length(x_values)) {
    distances <- c(distances, abs(coef(fit)[2]*x_values[i] - y_values[i] + coef(fit)[1]) / sqrt(coef(fit)[2]^2 + 1^2))
  }
  
  # Max distance point
  x_max_dist <- x_values[which.max(distances)]
  y_max_dist <- y_values[which.max(distances)]
  
  return(c(x_max_dist, y_max_dist, max(distances)))
}
#plot and show elbow graph to detemine uptimal number of cluseter
showElbowGraph <- function(x_clusters, y_wcss) {
  nb_wcss_values = length(y_wcss)
  extremes_line_coef = (x_clusters[nb_wcss_values] - x_clusters[1]) / (y_wcss[nb_wcss_values] - wcss_values[1])
  extremes_orth_line_coef = -1 / extremes_line_coef
  elbowPoint_orth_proj = c(elbowPoint_info[1] + elbowPoint_info[3]/2, elbowPoint_info[2] + extremes_orth_line_coef * (elbowPoint_info[3]/2))
  
  plot(x_clusters, 
       y_wcss, 
       type="b", pch = 19, 
       frame = FALSE,
       main = 'WCSS value according to the number of clusters', 
       xlab = 'Number of clusters', 
       ylab = 'WCSS value')
  lines(x=c(x_clusters[1], x_clusters[nb_wcss_values]), y=c(y_wcss[1], y_wcss[nb_wcss_values]), type="b", col='green')
  lines(x=c(elbowPoint_info[1], elbowPoint_orth_proj[1]), y=c(elbowPoint_info[2], elbowPoint_orth_proj[2]), type="b", col='red')
}

ApplKmeans <- function(scaled_data, nonscaled_data) {
  # Automatically choose the right K and applies Kmeans
  #
  #Args:
  #  scaled.data:  The output from DataPrep function. This is the scaled version
  #  nonscaled.data: The output from DataPrep function. This is the non scaled version
  #
  #Returns:
  #  Returns the non scaled output from DataPrep function with a cluster column
  #
  # Error handling 
  
  
  if (TRUE %in% is.na(scaled_data) ||
      TRUE %in% is.na(nonscaled_data)) {
    stop("Arguments must not have missing values.")
  }
  
  # Picking K ####
  wcss_values <- getWCSSData(scaled_data)
  nb_clusters <- seq(1, length(wcss_values), 1)
  print(nb_clusters)
  elbowPoint_info <-
    getElbowPoint(x_values = nb_clusters, y_values = wcss_values)
  
  # Clustering ####
  
  kmeans_cluster <- kmeans(
    scaled_data,
    elbowPoint_info[1],
    nstart = 250,
    algorithm="Lloyd"
  )
  
  # Attaching Cluster result to actual data ####
  data_result <- cardioARulesDf
  data_result$cluster <- kmeans_cluster$cluster %>%
    as.character()
  
  
  return(data_result)
}

gc()
#Run KMeans via the ApplKMeans function and examine the output
km_output <- ApplKmeans(scaled_cadio_df,cardioARulesDf)
str(km_output)
head(km_output)

library(factoextra)
km_output <- kmeans(scaled_cadio_df, centers = 2, nstart = 25, iter.max = 1000, algorithm = "MacQueen")
fviz_cluster(km_output, data = scaled_cadio_df)

confusionMatrix(factor(km_output$cluster), factor(cardioARulesDf$cardio, levels = c(0,1), labels = c(2,1)))

km_output$cluster

#get elbow point
# wcss_values <- getWCSSData(scaled_cadio_df)
# nb_clusters <- seq(1, length(wcss_values), 1)
# elbowPoint_info <-  getElbowPoint(x_values = nb_clusters, y_values = wcss_values)

#Plot the Elbow graph visualize optimal number of clusters
showElbowGraph(nb_clusters, wcss_values)

#Plot the Elbow graph visualize optimal number of clusters
showElbowGraph(nb_clusters, wcss_values)


#Loss function:Sum of Square Error

#Visualize Cluster Assignment
#We observe the 4 clusters with slight overlaps 
clusplot(cardioARulesDf,km_output$cluster,color=TRUE,shade=TRUE, labels=1,lines=1)

km_output %>% group_by(km_output$cadio,km_output$cluster) %>% summarise(number=n())

#Hierarchical Clustering using difernt interclustering distance
#complete linkage and euclidean distance method as we have clusters with small diameters
hac_output_comp <- hclust(dist(cardioARulesDf, method = "euclidean"), method = "complete")
hac_output_comp
plot(hac_output_comp,main = "Cluster Dendogram - Complete Linkage- ", label = cardioARulesDf$cadio)

##Average linkage and euclidean distance method
#To overcome sensitivity to outliers
hac_output_avg <- hclust(dist(cardioARulesDf, method = "euclidean"), method = "average")
hac_output_avg
plot(hac_output_avg,main = "Cluster Dendogram - Average Linkage- ", label = cardioARulesDf$cadio) 
