#Loading libraries
library(dplyr)

#loading the dataset
data1<-read.csv("/home/prasannprem_live/Projects/churn_data.csv")

data2<- data1[,-c(1,2,3)]

library(caTools)

#set the sees to reproduce my sample
set.seed(121)
sample<-sample.split(data2$churn, SplitRatio = 0.70)
sample
train_data<-subset(data2,sample==TRUE)
test_data<-subset(data2,sample==FALSE)

#install e1071 package for executing SVM Classifier
library(e1071)

#Building SVM classifier
classifier = svm(churn ~ ., data = train_data, type = 'C-classification') 
classifier

# Predicting the Test set results 
y_pred = predict(classifier, newdata = test_data) 
y_pred

# Making the Confusion Matrix 
m <- table(test_data[,17],y_pred)
m

#accuracy % 91.4%
(1276+95)/(1276+12+117+95)

#Bind the test data and prediction
final_data<- cbind(test_data,y_pred)

#export the file
write.csv(final_data,"SVM_telecom_churn_output.csv")
