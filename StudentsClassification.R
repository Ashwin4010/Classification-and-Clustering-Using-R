library(dplyr)
library(C50)
library(gmodels)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

rm(list=ls()) #clear workspace
set.seed(1234)

mydata <- "data/student_math.csv" 
mydata <- read.table(mydata, sep = ",", header = TRUE) #read data

numdata <- sapply(mydata, is.numeric) #seperate numeric variables
mydata <- mydata[ , numdata] #keep only numeric variables

mydata$result <- NULL
mydata$result <- factor(
  ifelse(mydata$G3 >= 10, 1, 0), 
  labels = c("fail", "pass")
)   #assign fail or pass to result


normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
} #creare a normalization function


mydata [, -17] <- as.data.frame(lapply(mydata[1:16], normalize))

mydata <- mydata[,-16] 

ind <- sample(2, nrow(mydata), replace=TRUE, prob=c(0.80, 0.20)) #creare sample of the data

mydata.training  <- mydata[ind==1,1:16]
mydata.test  <- mydata[ind==2,1:16]

m <- C5.0(x = mydata.training[-16], y = mydata.training$result , trials = 20)  #the model
p <- predict(m, mydata.test) #prediction model

#print results
CrossTable(mydata.test$result, p, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c("actual pass","predicted pass"))

#create tree using rpart, so we can plot it
m2 <- rpart(result ~ . , data = mydata.training, method = 'class')
#Plot it
prp(m2,varlen = 6, extra = 2)
