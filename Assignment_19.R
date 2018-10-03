1. Use the below given data set
DataSet
2. Perform the below given activities:
a. Create classification model using different classifiers
b. Verify model goodness of fit
c. Apply all the model validation techniques.

setwd("F:/AcadGild/workings")

lib=c("bigmemory", "readr", "Hmisc", "dplyr", "MASS", "ggplot2", "lattice", "caret", "rpart", 
      "randomForest", "rpart.plot","lattice", "rattle", "data.table","RColorBrewer", "reshape2",
      "InformationValue","stringr", "VIF", "Information", "Amelia", "gdata", "party","car", 
      "lubridate","zoo", "sqldf", "fuzzyjoin", "party", "mice")
sapply(lib, require, character.only=TRUE, quietly=TRUE)

Data<-fread("F:/AcadGild/workings/Example_WearableComputing_weight_lifting_exercises_biceps_curl_variations_19.csv", sep = ",", header = TRUE)

# exploratory analysis 
dim(Data)
str(Data)
summary(Data)
describe(Data)
head(Data)
sapply(Data, class) 
Amelia::missmap(Data) # 62% data has missing values 
class(Data)

# missing values
sapply(Data,function(x) sum(is.na(x)))

#Delete columns with all missing values
Data<-as.data.frame(Data) # conver to data frame
Training <-Data[,colSums(is.na(Data)) == 0]
dim(Training)

# deleting variables irrelevent to this project
Training<-Training[,-c(1:6)]

# data Partition so that 70% of it is in the TrainingTraining data set and the remaining 30% to TestingTraining
inTrainingSet <- createDataPartition(y=Training$classe, p=0.70, list=FALSE)
Train <- Training[inTrainingSet,]
Test <- Training[-inTrainingSet,]

#First Prediction Model - Decision Tree
FirstModel <- rpart(classe ~ ., data=Train, method="class")
print(FirstModel)
attributes(FirstModel)
plot(FirstModel)
summary(FirstModel)

#Predicting
FirstPrediction <- predict(FirstModel, Test, type = "class")
attributes(FirstPrediction)
plot(FirstPrediction)
summary(FirstModel)

#Using confusion Matrix to test results:
confusionMatrix(FirstPrediction, Test$classe)

#Second Prediction Model - Random Forests
Train$classe<-as.factor(Train$classe)
class(Train$classe)

rf <- randomForest(classe ~., data=Train, method ="class")
print(rf)
attributes(rf)
plot(rf)
summary(rf)

#Predicting:
SecondPrediction <- predict(rf, Test, type = "class")
summary(SecondPrediction)
attributes(SecondPrediction)
plot(SecondPrediction)

#Test results on TestingTraining data set:
confusionMatrix(SecondPrediction, Test$classe)

#Testing the better model on original Testing Set
FinalPrediction <- predict(rf, Train, type="class")
FinalPrediction

confusionMatrix(FinalPrediction, Train$classe)

describe(FinalPrediction)


# goodnes of fit - chi square test. 
final.table<-table(FinalPrediction)
final.table

barplot(final.table, xlab="class", ylim = c(0,1000))

final.proportion.tab<-(final.table/sum(final.table))
final.proportion.tab

barplot((final.proportion.tab)*100, main = "percentage of class", ylim = c(0,40))
chisq.test(final.table)

# validation techniques 
# split dataset into "training" (80%) and "validation" (20%)
ind<-sample(2, nrow(Data), replace=TRUE, prob = c(0.80, 0.20))
tdata<-Data[ind==1,]
vdata<-Data[ind==2,]

head(tdata)
head(vdata)

# multiple linear regression model
results<-lm(classe ~., data = tdata, model = "binomial")

summary (results)
results$coefficients




