rice_data <- read.csv('C:/Project DBs/DMML/riceClassification.csv',header = TRUE) # read data
sum(rowSums(is.na(rice_data))>0) # check number of NA values
names(rice_data) # column names
str(rice_data) # structure of the data
rice_data <- rice_data[,-1] #Drop id column
summary(rice_data) # summary of all columns
#pairs(rice_data[-1],main="Rice Data (red=1,green=0)",pch=21,bg=c("red","green")[unclass(rice_data$Class)])
library(corrplot)
correlation <- cor(rice_data) # correlation of the data
corrplot(correlation,type='lower') # correlation plot
names(rice_data) #names of columns
rice_data <- rice_data[-c(7,5,9,4)] #drop columns with high correlation between them
corrplot(cor(rice_data[,-7]),type='lower') # correlation plot
attach(rice_data) # attach the data
set.seed(100) # seed set for reproducibility
row_split <- sort(sample(nrow(rice_data), nrow(rice_data)*.7)) # row split identified
training_data <- rice_data[row_split,] # 70% training data
testing_data <- rice_data[-row_split,] #30% testing data
llm1 <- glm(Class~.,training_data,family='binomial') #logistic model with all variables as predictors
llm2 <- glm(Class~Area+MajorAxisLength+MinorAxisLength+EquivDiameter+AspectRation ,training_data,family='binomial') # Model with high p value predictors removed
fitted_values <- round(fitted(llm2)) # predicted values by the model for the training data rounded to 1 and 0
actual_response <- training_data$Class # actual values
table(fitted_values,actual_response) # tabular form of predicted vs actual 
predicted_values <- predict(llm2,newdata = testing_data,type='response') # predict values for test data
summary(predicted_values) # summary of the predicted values
actual_values <- testing_data$Class # actual values from test data
prediction_values <- rep("0",nrow(testing_data)) # dummy predicted values with all rows as 0
prediction_values[predicted_values>0.5] <- "1" # assigned values based on prediction
cm=table(prediction_values,testing_data$Class) #tabular form
cm
library(caret)
confusionMatrix(cm) #confusion matrix