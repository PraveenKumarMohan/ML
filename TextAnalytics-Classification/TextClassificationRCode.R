library(quanteda)
library(irlba)
library(e1071)
library(caret)

#import the input raw data
raw.data <- read.csv("spam.csv", stringsAsFactors = FALSE)

#remove unwanted columns
raw.data <- raw.data[,1:2]
names(raw.data) <- c("Label", "Text")

#check missing values
length(which(!complete.cases(raw.data)))

str(raw.data)
#Convert label to a factor variable
raw.data$Label <- as.factor(raw.data$Label)

#Class balance check
prop.table(table(raw.data$Label))

#Add a new column to have the length of the text
raw.data$TextLength <- nchar(raw.data$Text)
summary(raw.data)

#Visualize the distribution
library(ggplot2)

ggplot(raw.data, aes(x= TextLength, fill = Label)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Text Count", x = "Length of the Text",
       title = "Distribution of Text lengths")

#Create train and test data sets
library(caret)
set.seed(1234) #32984

idx <- createDataPartition(raw.data$Label, times = 1, p = 0.7, list = FALSE)
train <- raw.data[idx,]
test <- raw.data[-idx,]
