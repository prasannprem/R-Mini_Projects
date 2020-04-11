#Business Problem: To predict the healthcare cost
#Loading the libraries
library(dplyr)

#Loading the dataset
data1<-read.csv("/home/prasannprem_live/Projects/hospitalcosts.csv")

#Data exploration
dim(data1)
summary(data1)
unique(data1$RACE)
unique(data1$LOS)

#With reference to business problem and exploration, we can say that TOTCHG is dependent variable

#dividing data into training and testing

library(caTools)

# set the seed to reproduce my sample 
set.seed(1)

#sample the input data with 70% for training and 30% for testing
sample <- sample.split(data1$TOTCHG,SplitRatio = 0.70)
sample 
train_data<- subset( data1, sample==TRUE)
test_data<- subset(data1, sample == FALSE)

#Model Building: We will use Multiple linear regression model, since the data to be predicted is
#continuous and we have multiple variable to predict output
model<-lm(TOTCHG~.,data=train_data)
summary(model)

#rebuild the model using only significant variables, for those p-value is less than 0.05
final_model<-lm(TOTCHG~AGE+LOS+APRDRG,data=train_data)
summary(final_model)

#prediction on the unforseen dataset that is on testing data set 

predtest <- predict(final_model, test_data)
predtest
predtest1<-data.frame(predtest)

#plotting actual versus predicted values 
#plotting in graphs:we can see red nad blue lines are overlapping which shows best fit model
plot (test_data$TOTCHG,col="red",type ="l",lty=1.8 )
lines(predtest1,col="blue",type ="l",lty=1.4)

#to bind the predicted data set with original data set by cbind function
final_data<- cbind(test_data,predtest1)

#export
write.csv(final_data,"Healthcarecost_Prediction.csv")