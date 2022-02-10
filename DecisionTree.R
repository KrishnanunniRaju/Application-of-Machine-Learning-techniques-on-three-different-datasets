rice_data <- read.csv('C:/Project DBs/DMML/riceClassification.csv',header = TRUE) # Read data from csv
sum(rowSums(is.na(rice_data))>0) # check number of rows with na values
names(rice_data) # names of columns
str(rice_data) # structure of the data
rice_data <- rice_data[,-1] #Drop id column
summary(rice_data) #summary of the data
#pairs(rice_data[-1],main="Rice Data (red=1,green=0)",pch=21,bg=c("red","green")[unclass(rice_data$Class)])
library(corrplot) # corplot for correlation plot
correlation <- cor(rice_data) # compute correlation
corrplot(correlation,type='lower') # Correlation plot
rice_data$Class <- as.factor(rice_data$Class) # Class of rice is changed to factor from numeric
attach(rice_data) # data attached
set.seed(100) # seed set for reproducibility
row_split <- sort(sample(nrow(rice_data), nrow(rice_data)*.7)) #row split identified for splitting data into 70% and 30%
training_data <- rice_data[row_split,] # training data with 70%
testing_data <- rice_data[-row_split,] # testing data with 30%
library(ISLR2) 
library(tree)
tree.fit <- tree(Class~.,training_data) # tree model
summary(tree.fit) # summary of the model
tree.pred <- predict(tree.fit,testing_data,type='class') # predict value of test data
table(tree.pred,testing_data$Class) # table od predicted vs test
library(tree)
cv.tree.fit <- cv.tree(tree.fit,FUN=prune.misclass) #cross validation performed on the tree
names(cv.tree.fit) # check columns
cv.tree.fit # print the cross validation result
plot(cv.tree.fit$size,cv.tree.fit$dev,type='b') #plot tree size vs deviation
plot(cv.tree.fit$k,cv.tree.fit$dev,type='b') # plot k against deviation
prune.tree.fit <- prune.misclass(tree.fit,best=2) # prune tree for best fit
plot(prune.tree.fit) # plot tree
text(prune.tree.fit,pretty = 0) # add labels in tree
tree.pred <- predict(prune.tree.fit,testing_data,type='class') #predict using pruned tree
table(tree.pred,testing_data$Class) # Tabular form of prediction
