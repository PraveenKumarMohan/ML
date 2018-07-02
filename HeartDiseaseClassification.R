#Set the working directory and read the input file
setwd("C:/Study Materials/Kaggle/heart-disease-uci")
heartdata <- read.csv("heart.csv", header = T)

#Setting random seed
set.seed(1712)
str(heartdata)

#Cleaning the data
#Identifying the correct data types
heartdata$sex <- factor(heartdata$sex, levels = c(0,1), labels = c("Female", "Male"))
heartdata$cp <- as.factor(heartdata$cp)
heartdata$fbs <- factor(heartdata$fbs, levels = c(0,1), labels = c("False", "True"))
heartdata$exang <- factor(heartdata$exang, levels = c(0,1), labels = c("No", "Yes"))
heartdata$ca <- as.factor(heartdata$ca)
heartdata$thal <- factor(heartdata$thal, levels = c(1,2,3), labels = c("Normal", "Fixed Defect", "Reversable Defect"))
heartdata$target <- as.factor(heartdata$target)

summary(heartdata_n)

#To find the missing data
sapply(heartdata, function(x) sum(is.na(x)))
# To omit the 2 records that has missing values
heartdata_n <- na.omit(heartdata)
sapply(heartdata_n, function(x) sum(is.na(x)))

#To rename the column name
colnames(heartdata_n)[1] <- c("Age")

table(heartdata_n$target)

#Split Training and Testing datasets
library(caret)
idx <- createDataPartition(heartdata_n$target, p=0.75, list = FALSE)
train <- heartdata_n[idx,]
test <- heartdata_n[-idx,]

#Let's test the accuracy with everyone has the heart disease
a1 <- rep(1, dim(test)[1])
(accu1 <- 1-mean(a1 == test$target))
