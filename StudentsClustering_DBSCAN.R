library('fpc')
library('factoextra')
library('dbscan')

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

for_clust_data  <- as.data.frame(lapply(mydata[ ,c(5,6,13,14,15,16)], normalize)) #normalize data , keep only last 3 variables
d <- dbscan(for_clust_data,eps = 0.35, MinPts = 7,showplot = 2)#run dBSCAN

#plot results
plot(d, for_clust_data, main = "DBSCAN", frame = FALSE)
#fviz_cluster(d, for_clust_data, stand = FALSE, frame = FALSE, geom = "point")