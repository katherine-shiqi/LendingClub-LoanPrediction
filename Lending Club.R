# 2. Preparation of Model Building --------

# 2.1 remove the unexpected columns--------
data <- read.csv("/Users/shiqi/Desktop/Project/dataset/original_data.csv")
drop <- c("X","Unnamed..0","addr_state")
data[drop] <- NULL

# 2.2 Split dataset to training and testing (1:1)----
library(caret)
set.seed(1234)
splitIndex <- createDataPartition(data$loan_status, p = .50, list = FALSE, times = 1)
traindata <- data[ splitIndex,]
testdata <- data[-splitIndex,]
prop.table(table(traindata$loan_status))
dim(traindata) # training and testing data contain 51284 rows respectively

# 2.3 Solution to imbalance problem (SMOTE)-----
# oversampling and undersampling for the training dataset
library(DMwR)
traindata$loan_status <- as.factor(traindata$loan_status)
traindata <- SMOTE(loan_status ~ ., traindata, perc.over = 100, perc.under=200) 
write.csv(traindata,"/Users/shiqi/Desktop/Project/dataset/aftersmote.csv")
table(traindata$loan_status)
# numbers of "0" and "1" are equal after SMOTE with 15546 records each
dim(traindata)
# so the traindata currently contains 31092 rows


# 3. Model Training-------------------

# 3.1 Logistic Regression------------
lg.1 <- glm(loan_status~.,data=traindata, family="binomial")
traindata$pred1 <- predict(lg.1, newdata=traindata, type='response')
summary(lg.1)
library(pROC)
roc1 <- roc(traindata$loan_status, traindata$pred1, plot=TRUE, print.thres=TRUE)
print(roc1)
traindata$pred1 <- NULL
traindata$X <- NULL
# training AUC= 0.698
testdata$pred1 <- predict(lg.1, newdata=testdata, type='response')
roc2 <- roc(testdata$loan_status, testdata$pred1, plot=TRUE, print.thres=TRUE)
print(roc2)
# testing AUC= 0.6754
testdata$pred1.2[testdata$pred1<0.5] <- 0
testdata$pred1.2[testdata$pred1>=0.5] <- 1
mean(testdata$loan_status == testdata$pred1.2)
# testing Accuracy = 0.6228648
varImp(lg.1, scale=FALSE)
lg.1$coefficients


# 3.2 Support Vector Machine---------
library(e1071)
fit_svm <- svm(loan_status~., data=traindata)
testdata$pred3 <- predict(fit_svm, newdata = testdata, type="prob")
testdata$pred3 <- as.numeric(as.character(testdata$pred3))
roc(testdata$loan_status, testdata$pred3, plot=TRUE, print.thres=TRUE)
# testing AUC = 0.6243
mean(testdata$loan_status == testdata$pred3)
# testing Accuracy = 0.6325


# 3.3 Random Forest----------------
library(randomForest)
fit_rf <- randomForest(loan_status~., data=traindata, ntree=200)
testdata$pred5 <- predict(fit_rf, newdata = testdata)
testdata$pred5 <- as.numeric(as.character(testdata$pred5))
roc(testdata$loan_status, testdata$pred5, plot=TRUE, print.thres=TRUE)
# testing Auc = 0.6144
mean(testdata$loan_status == testdata$pred5)
# testing Accuracy = 0.7045472

# 3.4 Gradient Boosting -------------
fitControl <- trainControl(method="repeatedcv", number = 4, repeats = 4)
fit <- train(loan_status~., data=traindata, method="gbm", trControl = fitControl, verbose = FALSE)
testdata$pred2 <- predict(fit, newdata = testdata, type="prob")
roc(testdata$loan_status, testdata$pred2[, 2], plot=TRUE, print.thres=TRUE)
# testing AUC = 0.6793
testdata$pred2.3[testdata$pred2[, 2]<0.5] <- 0
testdata$pred2.3[testdata$pred2[, 2]>=0.5] <- 1
mean(testdata$loan_status == testdata$pred2.3)
# testing Accuracy = 0.7477966

# Conclusion: Gradient Boosting is selected as our model 
# based on its highest AUC and Accuracy


# 4. Financial Analysis-----------------

# 4.1 Cut-off Selection-------------
revenue_ls <- c()
ilist <- seq(0.5, 1, 0.05) #generate list from 0.5 to 1 with incremental of 0.05
revenue_ls <- c()

# run a loop tp calculate the profits for different cut-offs
for(i in ilist) {
  testdata$pred2.3[testdata$pred2[, 2]< i] <- 0
  testdata$pred2.3[testdata$pred2[, 2]>=i] <- 1
  revenue <- sum(testdata$loan_amnt[testdata$pred2.3==1 & testdata$loan_status==1]*testdata$int_rate[testdata$pred2.3==1 & testdata$loan_status==1])
  cost <- sum(testdata$loan_amnt[testdata$pred2.3==1 & testdata$loan_status==0])
  total_after <- revenue-cost
  revenue_ls <- c(revenue_ls,total_after)
}
ilist
# Plot the relationship of cut-off and profit
plot(ilist,revenue_ls, xlab = "cut-off value", ylab = "total profit")

# Plot the relationship of cut-off and profit
max(revenue_ls)

testdata$pred2.3[testdata$pred2[, 2]<0.7] <- 0
testdata$pred2.3[testdata$pred2[, 2]>=0.7] <- 1

# testing Accuracy = 0.4669098 after changing cut-off to 0.7
mean(testdata$loan_status == testdata$pred2.3)
confusionMatrix(data=testdata$pred2.3, testdata$loan_status)

# Column contribution
varImp(fit, scale=FALSE)
plot(testdata$mort_acc,testdata$pred2[, 2])

# 4.2 Cost-Benefit Analysis-------------
#Profit Before Our Project 
total_before <- sum(testdata$loan_amnt[testdata$loan_status==1]*testdata$int_rate[testdata$loan_status==1]) - sum(testdata$loan_amnt[testdata$loan_status==0])
# total_before/ #investor = $ -593 

# Project After Our Project 
revenue_after <- sum(testdata$loan_amnt[testdata$pred2.3==1 & testdata$loan_status==1]*testdata$int_rate[testdata$pred2.3==1 & testdata$loan_status==1])
cost_after <- sum(testdata$loan_amnt[testdata$pred2.3==1 & testdata$loan_status==0])
total_after <- revenue_after - cost_after
# total_before/ #investor = $ 327 


# 5. New Data Prediction-----------

# new data needed to predict
predictdata <- read.csv("/Users/shiqi/Desktop/Project/dataset/newdata.csv")
predictdata$X <- NULL
predictdata$Unnamed..0 <- NULL
# predict the data based on gradient boosting
predictdata$prob_loan_status <- predict(fit, newdata = predictdata, type="prob")
predictdata$loan_status[predictdata$prob_loan_status[, 2]<0.7] <- 0
predictdata$loan_status[predictdata$prob_loan_status[, 2]>=0.7] <- 1
predictdata$prob_loan <- predictdata$prob_loan_status[, 2]
predictdata$prob_loan_status <- NULL
# write the csv
write.csv(predictdata,"/Users/shiqi/Desktop/Project/dataset/afterpredict.csv")
sum(is.na(predictdata))


# 6. Data Preparation for Shiny--------

after <- read.csv("/Users/shiqi/Desktop/Project/dataset/afterpredict.csv")
hist(after$prob_loan)
# generate the column of "expected return rate" 
after$exp_return <- after$int_rate * after$prob_loan
# generate the column of "up_boundary" 
after$up_boundary <- 0.2403
after$up_boundary[after$grade == 'E'] <- 0.2007
after$up_boundary[after$grade == 'D'] <- 0.1727
after$up_boundary[after$grade == 'C'] <- 0.1416
after$up_boundary[after$grade == 'B'] <- 0.1095
after$up_boundary[after$grade == 'A'] <- 0.0743
# generate the column of "low_boundary" 
after$low_boundary <- 0.0523
after$low_boundary[after$grade == 'E'] <- 0.0634
after$low_boundary[after$grade == 'D'] <- 0.0682
after$low_boundary[after$grade == 'C'] <- 0.0710
after$low_boundary[after$grade == 'B'] <- 0.0656
after$low_boundary[after$grade == 'A'] <- 0.0495
# write the csv for shiny APP
write.csv(after, "/Users/shiqi/Desktop/Project/dataset/shinydata.csv")
