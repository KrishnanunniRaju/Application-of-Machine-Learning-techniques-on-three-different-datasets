rice_data <- read.csv('C:/Project DBs/DMML/riceClassification.csv',header = TRUE) # read the data
sum(rowSums(is.na(rice_data))>0) # check number of null values
names(rice_data) # names of columns
str(rice_data) # structure of data
rice_data <- rice_data[,-1] #Drop id column
summary(rice_data) # summary of data
#pairs(rice_data[-1],main="Rice Data (red=1,green=0)",pch=21,bg=c("red","green")[unclass(rice_data$Class)])
library(corrplot) 
correlation <- cor(rice_data) # correlation of the data frame
corrplot(correlation,type='lower') # correlation plot
rice_data$Class <- as.factor(rice_data$Class) # class converted to factor from numeric
attach(rice_data) # attached data
set.seed(100) #seed set for reproducibility
row_split <- sort(sample(nrow(rice_data), nrow(rice_data)*.7)) # row split identified for testing and training data
training_data <- rice_data[row_split,] # training data with 70% data
testing_data <- rice_data[-row_split,] # testing data with 30% data
library(e1071) 
nbFit <- naiveBayes(training_data[,-1],training_data[,11]) # naive bayes model creation
nbFit # check the model
predictions <- predict(nbFit,newdata = testing_data) # predict with test data
cm <- table(predictions,testing_data$Class) # table view of predicted vs tested
cm 
library(caret)
confusionMatrix(cm) # confusion matrix of the fit
library(gmodels)
CrossTable(predictions, testing_data$Class, prop.chisq = FALSE, prop.t = FALSE, dnn = c('predicted', 'actual')) # cross table of predicted vs test
roc.curve(testing_data$Class,predictions) # ROC curve of the fit

