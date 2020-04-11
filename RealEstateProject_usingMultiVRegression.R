library(dplyr)
data1<-read.csv("regression_data.csv")
View(data1)

#creating factor objects
factor_rooms<-as.factor(data1$number.of.rooms)
factor_gar<-as.factor(data1$number.of.garages)
factor_bedr<-as.factor(data1$number.of.bedrooms)

#dataexploration
head(data1)
summary(data1)
dim(data1)

#dividing data into training and testing

library(caTools)

# set the seed to reproduce my sample 
set.seed(1)

#sample the input data with 70% for training and 30% for testing
sample <- sample.split(data1$selling.price,SplitRatio = 0.70)
sample 
train_data<- subset( data1, sample==TRUE)
test_data<- subset(data1, sample == FALSE)

str(data1)

#MOdel building
model<-lm(selling.price~.,data=train_data)
summary(model)

#rebuild the model using only significant variables
final_model<-lm(selling.price ~ local.selling.price.in.hundred.of.dollars + number.of.bathrooms 
           +size.of.the.living.space + number.of.garages + construction.type , data = train_data)
summary(final_model)

#rebuild the model once again since p-value for no of garages is greater than 0.05
final_model1<-lm(selling.price ~ local.selling.price.in.hundred.of.dollars + number.of.bathrooms 
                +size.of.the.living.space + construction.type , data = train_data)
summary(final_model1)

#plot lm model
require(ggplot2)
ggplot(train_data,aes(y=selling.price,x=local.selling.price.in.hundred.of.dollars,color=number.of.bathrooms))+geom_point()+geom_smooth(method="lm")


#prediction on the unforseen dataset that is  on testing data set 

predtest <- predict(final_model, test_data)
predtest
predtest1<-data.frame(predtest)

#plotting actual versus predicted values 
#plotting in graphs:we can see red nad blue lines are overlapping which shows best fit model
plot (test_data$selling.price,col="red",type ="l",lty=1.8 )
lines(predtest1,col="blue",type ="l",lty=1.4)

#to bind the predicted data set with original data set by cbind function
final_data<- cbind(test_data,predtest1)

#export
write.csv(final_data,"realestate_lregre_output.csv")
