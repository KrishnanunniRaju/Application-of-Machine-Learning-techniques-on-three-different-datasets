bank_data <- read.csv('C:/Project DBs/DMML/Bank/train.csv',header = TRUE,stringsAsFactors = TRUE) #read data from CSV file with 1st row as column names and all strings converted to factors
str(bank_data) #To examine the structure of the data
bank_data$Loan.Status <- as.factor(bank_data$Loan.Status) #Converted Loan status to a factor with 2 levels
levels(bank_data$Loan.Status) #Check levels to verify
cleaned_data <- na.omit(bank_data) # Remove rows with NA values
# Chi-squared test done with all the other fac?ors and non significant are dropped
chisq.test(cleaned_data$Batch.Enrolled,cleaned_data$Loan.Status,simulate.p.value = TRUE)
chisq.test(cleaned_data$Grade,cleaned_data$Loan.Status,simulate.p.value = TRUE)
chisq.test(cleaned_data$Application.Type,cleaned_da?a$Loan.Status,simulate.p.value = TRUE)
chisq.test(cleaned_data$Initial.List.Status,cleaned_data$Loan.Status,simulate.p.value = TRUE)#
chisq.test(cleaned_data$Loan.Title,cleaned_data$Loan.Status,simulate.p.value = TRUE)
chisq.test(cleaned_data$Verification.Status,cleaned_data$Loan.Status,simulate.p.value = TRUE)
chisq.test(cleaned_data$Employment.Duration,cleaned_data$Loan.Status,simulate.p.value = TRUE)#
chisq.test(cleaned_data$Sub.Grade,cleaned_data$Loan.Status,simulate.p.value = TRUE)
library(dplyr) # dply? library loaded
cleaned_data <- cleaned_data%>%select(-c(6,8,9,12:14,29)) # Remove columns identified using Chi-squared test
str(cleaned_data) # Structure of the cleaned data examined
summary(cleaned_data) # Summary of the dataframe
names(cleaned_data) # A?l column names of the data frame
cleaned_data <- cleaned_data[c(-24,-1)] # Dropped id and a column payment.plan with constant value
# Factors converted to Numeric for KNN
cleaned_data$Initial.List.Status <- as.numeric(cleaned_data$Initial.List.Status) 
cleaned_data$Employment.Duration <- as.numeric(cleaned_data$Employment.Duration)
cor=cor(cleaned_data[sapply(cleaned_data,is.numeric)]) # Obtain correlation between all numeric data
cor # print correlations
library(corrplot) # Library corrplot loaded
corrplot(cor,type = 'lower') # Correlation plot 
cleaned_data$Loan.Status <- as.factor(cleaned_data$Loan.Status) # Loan status converted to factor
hist(as.numeric(cleaned_data$Loan.Status)) # Examine distribution of response variable to check for class imbalance
library(ROSE) # Library ROSE loaded
set.seed(50) # seed set for reproducing results
sampling_data <- ovun.sample(Loan.Status~.,data=cleaned_data,method='under',p=0.50)$data # Undersampling done using ROSE
sampling_data$Loan.Status <- ifelse(sampling_data$Loan.Status==1,0,1) # Loan status converted to 1 and 0 levels
library(caTools) # Library caTools loaded
row_split <- sample.split(sampling_data$Loan.Status,SplitRatio = 0.7) # Identified row split for sampling with 70% and 30% ssplit ratio
training_data <- subset(sampling_data,row_split=='TRUE') # subset the training data
testing_data <- subset(sampling_data,row_split=='FALSE') # subset the testing data
library(gmodels) # Library gmodels loaded
library(caret) # Library caret loaded
library(class)
set.seed(1) # seed set for repro?ucibility
knn.fit1 <- knn(train=training_data,test=testing_data,cl=training_data$Loan.Status) # knn classification using k=1
summary(knn.fit1) 
confusionMatrix(table(knn.fit1,testing_data$Loan.Status)) # Confusion matrix for k=1
mean(knn.fit1==testing_data$Loan.Status) # Accuracy
knn.fit3 <- knn(train=training_data,test=testing_data,cl=training_data$Loan.Status,k=3) # k=3
confusionMatrix(table(knn.fit3,testing_data$Loan.Status)) # Confusion matrix for k=3
mean(knn.fit3==testing_data$Loan.Status)# Accuracy
knn.fit5 <- knn(train=training_data,test=testing_data,cl=training_data$Loan.Status,k=5) #k=5
confusionMatrix(table(knn.fit5,testing_data$Loan.Status)) # Confusion matrix for k=5
mean(knn.fit5==testing_data$Loan.Status)# Accuracy
knn.fit10 <- knn(train=training_data,test=testing_data,cl=training_data$Loan.Status,k=10)
confusionMatrix(table(knn.fit10,testing_data$Loan.Status)) # Confusion matrix for k=10
mean(knn.fit10==testing_data$Loan.Status)# Accuracy
knn.fit50 <- knn(train=training_data,test=testing_data,cl=training_data$Loan.Status,k=50)
CrossTable(knn.fit50,testing_data$Loan.Status,prop.chisq = FALSE) # Table view of predicted and actual values
mean(knn.fit50==testing_data$Loan.Status)# Accuracy
confusionMatrix(table(knn.fit50,testing_data$Loan.Status)) # Co?fusion matrix for k=50
# Scaling needs to be done for the data
standardized_data <- scale(sampling_data[,-26]) # Scaling the data by excluding variable to be predicted
standardized_data <- as.data.frame(standardized_data) # Matrix converted to data frame
standardized_data$Loan.Status <- sampling_data$Loan.Status # Loan status column appended to the standardized values dataframe
train_data <- subset(standardized_data,row_split=='TRUE') # subset training data from standardized values data frame
test_data <- subset(standardized_data,row_split=='FALSE') # subset testing data from standardized values data frame
set.seed(1) # seed set for reproducibility
knn.pred1 <- knn(train_data,test_data,train_data$Loan.Status,k=1) #knn with k=1
mean(knn.pred1==test_data$Loan.Status) #accuracy
confusionMatrix(table(knn.pred1,test_data$Loan.Status)) # Confusion matrix to observe sensitivity and specificity
knn.pred10 <- knn(train_data,test_data,train_data$Loan.Status,k=10)#knn with k=10
mean(knn.pred10==test_data$Loan.Status)#accur?cy
confusionMatrix(table(knn.pred10,test_data$Loan.Status))# Confusion matrix to observe sensitivity and specificity
knn.pred50 <- knn(train_data,test_data,train_data$Loan.Status,k=50)#knn with k=50
mean(knn.pred50==test_data$Loan.Status)#accuracy
confusionMatrix(table(knn.pred50,test_data$Loan.Status))# Confusion matrix to observe sensitivity and specificity
library(pROC)
ROC <- roc(as.numeric(test_data$Loan.Status),as.numeric(knn.pred50)) # Receiver operating characteristic calculated
plot(ROC,type="bars") # ROC curve p?ot
auc(ROC) # Area under ROC curve computed