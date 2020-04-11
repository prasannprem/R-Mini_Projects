library(dplyr)
data1<-read.csv("Remission1.csv")
summary(data1)
str(data1)

set.seed(6) #to reproduce the sample: 

#splitting data set into training and validation
train_obs <- floor (0.7*nrow (data1))
print(train_obs)


train_ind<-sample(seq_len(nrow(data1)),size=train_obs)
test = -train_ind

train_data<-data1[train_ind,]#no of obs in train dataset
test_data<-data1[-train_ind,]# no of obs in test dataset

testing_high <- data1$Remiss[test]
testing_high

#MOdel building
regmodel<-glm(Remiss~.,data=train_data,family=binomial())
summary(regmodel)

#now we build model using only significant variables
final_model<-glm(Remiss~Cell+Li+Blast+Temp,data=train_data,family=binomial())
summary(final_model)

#We will drop Blast variable, as its p-value>0.05, so now it is not significant, and re-run the model
final_model1<-glm(Remiss~Cell+Li+Temp,data=train_data,family=binomial())
summary(final_model1)

#Now all our variables are significant, so we will predict the outcome on test set
prob_values <- predict(final_model1, test_data,type="response")
prob_values
prob1<-data.frame(prob_values)

#setting the cutoff for probability values

result<- ifelse(prob1> 0.5 , 1, 0)

result


#confusion matrix: performance of classification algorithm

table(result , testing_high)

#accuracy : 82%, if the accuracy>70% then good model else bad

(17+10)/(17+3+3+10)

#to bind the predicted data set with original data set by cbind function
final_data<- cbind(test_data,prob1)

#export
write.csv(final_data,"logistic_healthcare_output.csv")
