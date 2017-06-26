library('fpc')
library('factoextra')
library(dplyr)
library('magrittr')
 

rm(list=ls()) #clear workspace
set.seed(1234)

mydata <- "data/student_math.csv" 
mydata <- read.table(mydata, sep = ",", header = TRUE) #read data

mydata <- subset(mydata, G3>0) #exclude values with G3=0

numdata <- sapply(mydata, is.numeric) #seperate numeric variables
mydata <- mydata[ , numdata] #keep only numeric variables


normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
} #create a normalization function

for_clust_data  <- as.data.frame(lapply(mydata[ ,c(5,6,13,14,15,16)], normalize)) #normalize data , keep only 6 variables
km1 = kmeans(for_clust_data, 5, nstart=100)

kclusts <- data.frame(k=1:9) %>% group_by(k) %>% do(kclust=kmeans(for_clust_data, .$k))
clusters <- kclusts %>% group_by(k) %>% do(tidy(.$kclust[[1]]))
assignments <- kclusts %>% group_by(k) %>% do(augment(.$kclust[[1]], for_clust_data))
clusterings <- kclusts %>% group_by(k) %>% do(glance(.$kclust[[1]]))


# Plot results
plot(for_clust_data, col =(km1$cluster +1) , main="K-Means result with 5 clusters", pch=20, cex=2)
