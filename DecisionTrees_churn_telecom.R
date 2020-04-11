library(dplyr)
data1<-read.csv("/home/prasannprem_live/Projects/churn_data.csv")
summary(data1)

#Drop first 3 columns as they do not serve our business problem
data2<-data1[,-c(1,2,3)]

#dividing data into training and testing

library(caTools)

# set the seed to reproduce my sample 
set.seed(2)

#sample the input data with 70% for training and 30% for testing
sample <- sample.split(data2$churn,SplitRatio = 0.70)
sample 
train_data<- subset( data2, sample==TRUE)
test_data<- subset(data2, sample == FALSE)

#Model building
library(rpart)
churn_model<-rpart(churn~.,data=train_data)
churn_model

#to display it in diagram


library(rattle)

library(rpart.plot)

fancyRpartPlot(churn_model)

#Prediction
pred<-predict(churn_model,test_data,type="class")
pred

pred1<- data.frame(pred)

#Validation of result
#evaluate Confusion matrix

table(test_data$churn,pred)

#Accuracy of the model=
(1275+161)/(1275+161+13+51)

#to bind the predicted data set with original data set by cbind function
final_data<- cbind(test_data,pred1)

#export
write.csv(final_data,"Decisiontree_telecom_churn.csv")
