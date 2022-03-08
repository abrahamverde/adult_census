if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org")


library(readr)
library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(dplyr)
library(ranger)



#LOAD CSV DATA SET FROM MY GITHUB ACCOUNT
datasetURL <- "https://github.com/abrahamverde/adult_census/raw/master/adult.csv"
#
rawDataSet <- read.csv(datasetURL)


##DATA EXPLORATION
head(rawDataSet, 15)

nrow(rawDataSet)

ncol(rawDataSet)

names(rawDataSet)

str(rawDataSet)


#PEOPLE WITH THEIR INCOME
peopleIncome <- table(rawDataSet$income)

#INCOME RATE
peopleIncome_rate <- prop.table(peopleIncome)


#GRAPHS INCOME
barplot(peopleIncome,main = 'INCOME AND QUANTITY OF PEOPLE',ylab ='People')
barplot(peopleIncome_rate,main = 'INCOME AND QUANTITY OF PEOPLE - RATE',ylab ='Rate')



#INCOME BY AGE
ggplot(rawDataSet) + aes(x=age, group=income, fill=income) + 
  geom_histogram(binwidth=1, color='black')+
  labs(x="Age",y="Count",title = "INCOME BY AGE")


#INCOME BY SEX
ggplot(data=rawDataSet,
       aes(age,group=sex,fill=sex))+
  geom_histogram(binwidth=1, color='red')+ ggtitle('INCOME BY SEX')


#THE EXPLORATION MAKES ME REALIZED THERE ARE FEW REALLY RELEVANT FIELDS IN THE PREDICTIONS
#I'M GOING TO SELECT JUST THE RELEVANT FIELDS
cleanData <- rawDataSet %>% select(income, sex, age)

head(cleanData, 50)


#CHECK FOR NA
colSums(is.na(cleanData))


str(cleanData)


#I realized the characters inside income field could be a problem in the next steps. I decided to change name to these factors.
levels(cleanData$income)<-c("lower50", "higher50")
str(cleanData)


#CREATE DATA PARTITON
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(cleanData$income, times = 1, p = 0.3, list = FALSE)
train_set <- cleanData[-test_index,]
test_set <- cleanData[test_index,]

#EXPLORING DATA PARTITION
nrow(train_set)
nrow(test_set)
head(train_set, 15)
head(test_set, 15)
table(train_set$income)

#OBJECT TO CONTROL THE TRAINING
trainControlObject <- trainControl(method="cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

#FIT THE MODEL - GLM
fit_glm <- train(income~., data = train_set, trControl=trainControlObject, family = binomial, method = "glm", metric="ROC")


#RANDOM FOREST USING RANGER PACKAGE (THIS IS FASTER THAN OLDER PACKAGE "randomForest" )
fit_randomforest <- train(income~., data = train_set, method = "ranger", metric="ROC",num.trees=50,
                          trControl=trainControlObject)


#TABLE OF FINAL RESULTS
Results <- resamples(list(GLM=fit_glm, RANDOM_FOREST=fit_randomforest))

#SHOW SUMMARY OF RESULTS
summary(Results)



