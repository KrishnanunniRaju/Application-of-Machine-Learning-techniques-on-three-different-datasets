fifa_data <- read.csv('C:/Project DBs/fifa/data.csv',header=TRUE) # read the data
names(fifa_data) # check column names
cleaned_data <- fifa_data[-1] # drop id column
cleaned_data <- cleaned_data[cleaned_data$Position!="GK",] # select all outfield players
required_columns <- c("Overall","Age","Potential","Crossing", "Finishing", "HeadingAccuracy", "ShortPassing","Volleys","Dribbling","Curve","FKAccuracy","LongPassing","BallControl", "Acceleration", "SprintSpeed" ,"Agility" ,"Reactions", "Balance" ,"ShotPower", "Jumping", "Stamina", "Strength","LongShots", "Aggression","Interceptions","Positioning","Vision","Penalties","Composure","Marking","StandingTackle","SlidingTackle","GKDiving","GKHandling","GKKicking","GKPositioning","GKReflexes") # choose requried columns 
cleaned_data <- cleaned_data[,which(names(cleaned_data) %in% required_columns)] # select the required columns
cleaned_data <- na.omit(cleaned_data) # remove na values
library(ggplot2) 
ggplot(data=cleaned_data,aes(Overall))+geom_boxplot(fill='red')+theme_dark() # box plot of response variable
Quantile <- quantile(cleaned_data$Overall,probs=c(.25,.75),na.rm=FALSE) # quantile calculated 
iqr <- IQR(cleaned_data$Overall) # inter quartile range 
cleaned_data <- subset(cleaned_data,cleaned_data$Overall>Quantile[1]-1.5*iqr & cleaned_data$Overall<Quantile[2]+1.5*iqr) # outliers removed using inter quartile range method
#pairs(cleaned_data)
lm.fit <- lm(Overall ~ Age + Potential + Crossing + Finishing + HeadingAccuracy + ShortPassing + Volleys + Dribbling + Curve + FKAccuracy + LongPassing + BallControl + Acceleration + SprintSpeed + Agility + Reactions + Balance + ShotPower + Jumping + Stamina + Strength + LongShots + Aggression + Interceptions + Positioning + Vision + Penalties + Composure + Marking + StandingTackle + SlidingTackle + GKDiving + GKHandling + GKKicking + GKPositioning + GKReflexes,data=cleaned_data) # linear model creation
summary(lm.fit) # summary of model
model2 <- update(lm.fit,.~.-Curve-Interceptions-LongPassing-Volleys-GKPositioning-Dribbling-Jumping-Aggression-GKHandling-GKKicking-Agility) # updated model by removing columns with high p value
summary(model2) # summary of the model
par(mfrow=c(2,2)) # align model plots
plot(model2) # plot the model
library(car)
ncvTest(model2)
durbinWatsonTest(model2)
vif(model2)
library(caTools)
row_split <- sample.split(cleaned_data$Overall,SplitRatio = 0.7) # row split identified 
training_data <- subset(cleaned_data,row_split=='TRUE') # training data with 70% data
testing_data <- subset(cleaned_data,row_split=='FALSE') #testing data with 30% data
library(FNN)
library(Metrics)
pred1 <- knn.reg(train=training_data,test=testing_data,y=training_data$Overall,k=1) #knn regression model training data with testing data as test data
rmse(pred1$pred,testing_data$Overall) # Root mean squared error value of training data with testing data
pred1train <- knn.reg(train=training_data,test=training_data,y=training_data$Overall,k=1)#knn regression model training data with training data as test data
rmse(pred1train$pred,training_data$Overall)# Root mean squared error value of training data with training data

pred3 <- knn.reg(train=training_data,test=testing_data,y=training_data$Overall,k=3)#knn regression model training data with testing data as test data
rmse(pred3$pred,testing_data$Overall)# Root mean squared error value of training data with testing data
pred3train <- knn.reg(train=training_data,test=training_data,y=training_data$Overall,k=3)#knn regression model training data with training data as test data
rmse(pred3train$pred,training_data$Overall)# Root mean squared error value of training data with training data

pred5 <- knn.reg(train=training_data,test=testing_data,y=training_data$Overall,k=5)#knn regression model training data with testing data as test data
rmse(pred5$pred,testing_data$Overall)# Root mean squared error value of training data with testing data
pred5train <- knn.reg(train=training_data,test=training_data,y=training_data$Overall,k=5)#knn regression model training data with training data as test data
rmse(pred5train$pred,training_data$Overall)# Root mean squared error value of training data with training data

pred10 <- knn.reg(train=training_data,test=testing_data,y=training_data$Overall,k=10)#knn regression model training data with testing data as test data
rmse(pred10$pred,testing_data$Overall)# Root mean squared error value of training data with testing data
pred10train <- knn.reg(train=training_data,test=training_data,y=training_data$Overall,k=10)#knn regression model training data with training data as test data
rmse(pred10train$pred,training_data$Overall)# Root mean squared error value of training data with training data

pred20 <- knn.reg(train=training_data,test=testing_data,y=training_data$Overall,k=20)#knn regression model training data with testing data as test data
rmse(pred20$pred,testing_data$Overall)# Root mean squared error value of training data with testing data
pred20train <- knn.reg(train=training_data,test=training_data,y=training_data$Overall,k=20)#knn regression model training data with training data as test data
rmse(pred20train$pred,training_data$Overall)# Root mean squared error value of training data with training data
