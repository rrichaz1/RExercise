#Step 1 -load data
install.packages("ISLR")
library(ISLR)
attach(Smarket)
summary(Smarket)
cor(Smarket[,-9])
#Step 2- split into training and test data set
training= (Year < 2005)
testing = !training
training_data <- Smarket[training,]
testing_data <- Smarket[testing,]
Direction_testing = Direction[testing]
#Fil logistic model
stock_mod = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=training_data, family=binomial)
summary(stock_mod)
#Predict test model
model_predicted_probs = predict(stock_mod, testing_data, type="response") 
model_predicted_direction = rep("Down", 252)
#if probability > 0.5 make it up else leave as down 
model_predicted_direction[model_predicted_probs > 0.5] = "Up"

#Create confusion matrix to compare actual vs predicted
table(model_predicted_direction,Direction_testing)
#misclassification error
mean(model_predicted_direction != Direction_testing)
