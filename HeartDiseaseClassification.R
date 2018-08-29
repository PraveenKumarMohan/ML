#Set the working directory and read the input file
setwd("C:/Study Materials/Machine Learning/heart-disease-uci")
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

#Correlations of all the numeric fields
str(heartdata_n)
cor(subset(heartdata_n,select = c(1,4,5,7,8,10,11)))

#Visualizations of the dependent variable against categorical variables
spineplot(heartdata_n$cp, heartdata_n$target)
spineplot(heartdata_n$sex, heartdata_n$target)

#Visualization of the dependent variable against numerical variables
boxplot(heartdata_n$Age ~ heartdata_n$target)
boxplot(heartdata_n$chol ~ heartdata_n$sex)

#thal vs target
(facts <- table(heartdata_n$thal, heartdata_n$target))
colnames(facts) <- c("Normal", "Affected")
barplot(facts, main = "thal vs target",legend = row.names(facts))

#Performance of C5.0
library(C50)

cfifty <- C5.0(target ~ ., data = train, trials = 100)
cfiftyPrediction <- predict(cfifty, newdata = test[,-14])
(cfiftyAccuracy <- 1-mean(cfiftyPrediction != test$target))

#Performance of logistic regression
logist <- glm(target ~ ., family = binomial(link = "logit"), data = train)
summary(logist)
plot(logist)

anova(logist, test = "Chisq")
library(pscl)
pR2(logist)
logistPrediction <- predict(logist, newdata = test[,-14], type = 'response')

result1.logist <- ifelse(logistPrediction > 0.5,1,0)
(result1.Accuracy <- 1 - mean(result1.logist != test$target))

result2.logist <- ifelse(logistPrediction > 0.6,1,0)
(result2.Accuracy <- 1 - mean(result2.logist != test$target))

result3.logist <- ifelse(logistPrediction > 0.75,1,0)
(result3.Accuracy <- 1 - mean(result3.logist != test$target))


#Performance of Random Forest
library(randomForest)
forest <- randomForest(target ~ ., data = train, importance = TRUE, ntree = 2000)
varImpPlot(forest)
rfPrediction <- predict(forest, newdata = test[,-14], type = "class")
(rfAccuracy <- 1 - mean(rfPrediction != test$target))

#Performance of Conditional Inference Trees
library(partykit)
cTree <- ctree(target ~ ., data = train)
print(cTree)
plot(cTree, type = "simple")

cForest <- cforest(target ~ ., data = train)
cForestPrediction <- predict(cForest, newdata = test[,-14])
(cForestAccuracy <- 1 - mean(cForestPrediction != test$target))