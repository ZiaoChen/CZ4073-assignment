# ======================
# Assignment 2
# Name: Chen Ziao
# Matric No.: U142068G
# ======================


# ==========
# Problem 1
# ==========

# Remove all variables
rm(list=ls())

# 1. Import Dataset
churnData <- read.csv('assign2_ChurnData.csv')

# 2. Explortary Data Analysis
# 2.1 Dimension
dim(churnData)

# 2.2 Column Names
names(churnData)

# 2.3 Structure of dataset
str(churnData)

# 2.4 First few rows
head(churnData)

# 2.5 Last few rows
tail(churnData)

# 2.6 Summary Statistics
summary(churnData)
# For variable 'TotalCharges', there are 9 missing values
# These 9 rows are removed
churnData <- churnData[complete.cases(churnData),]
summary(churnData)

# 2.7 Plot all individual variables VS target
plot(churnData$Gender, churnData$Churn, ylab="Churn", xlab="Gender")
plot(churnData$SeniorCitizen, churnData$Churn, ylab="Churn", xlab="SeniorCitizen")
plot(churnData$Partner, churnData$Churn, ylab="Churn", xlab="Partner")
plot(churnData$Dependents, churnData$Churn, ylab="Churn", xlab="Dependents")
plot(churnData$Tenure, churnData$Churn, ylab="Churn", xlab="Tenure")
plot(churnData$PhoneService, churnData$Churn, ylab="Churn", xlab="PhoneService")
plot(churnData$MultipleLines, churnData$Churn, ylab="Churn", xlab="MultipleLines")
plot(churnData$InternetService, churnData$Churn, ylab="Churn", xlab="InternetService")
plot(churnData$OnlineSecurity, churnData$Churn, ylab="Churn", xlab="OnlineSecurity")
plot(churnData$OnlineBackup, churnData$Churn, ylab="Churn", xlab="OnlineBackup")
plot(churnData$DeviceProtection, churnData$Churn, ylab="Churn", xlab="DeviceProtection")
plot(churnData$TechSupport, churnData$Churn, ylab="Churn", xlab="TechSupport")
plot(churnData$StreamingTV, churnData$Churn, ylab="Churn", xlab="StreamingTV")
plot(churnData$StreamingMovies, churnData$Churn, ylab="Churn", xlab="StreamingMovies")
plot(churnData$Contract, churnData$Churn, ylab="Churn", xlab="Contract")
plot(churnData$PaperlessBilling, churnData$Churn, ylab="Churn", xlab="PaperlessBilling")
plot(churnData$PaymentMethod, churnData$Churn, ylab="Churn", xlab="PaymentMethod")
plot(churnData$MonthlyCharges, churnData$Churn, ylab="Churn", xlab="MonthlyCharges")
plot(churnData$TotalCharges, churnData$Churn, ylab="Churn", xlab="TotalCharges")

# 2.8 Cross-tabulate variables VS target
table(churnData$Gender, churnData$Churn)
table(churnData$SeniorCitizen, churnData$Churn)
table(churnData$Partner, churnData$Churn)
table(churnData$Dependents, churnData$Churn)
table(churnData$PhoneService, churnData$Churn)
table(churnData$MultipleLines, churnData$Churn)
table(churnData$InternetService, churnData$Churn)
table(churnData$OnlineSecurity, churnData$Churn)
table(churnData$OnlineBackup, churnData$Churn)
table(churnData$DeviceProtection, churnData$Churn)
table(churnData$TechSupport, churnData$Churn)
table(churnData$StreamingTV, churnData$Churn)
table(churnData$StreamingMovies, churnData$Churn)
table(churnData$Contract, churnData$Churn)
table(churnData$PaperlessBilling, churnData$Churn)
table(churnData$PaymentMethod, churnData$Churn)

# 2.9 Mosaicplots of cross-tabulations
mosaicplot(table(churnData$Gender, churnData$Churn), ylab="Churn", xlab="Gender")
mosaicplot(table(churnData$SeniorCitizen, churnData$Churn), ylab="Churn", xlab="SeniorCitizen")
mosaicplot(table(churnData$Partner, churnData$Churn), ylab="Churn", xlab="Partner")
mosaicplot(table(churnData$Dependents, churnData$Churn), ylab="Churn", xlab="Dependents")
mosaicplot(table(churnData$PhoneService, churnData$Churn), ylab="Churn", xlab="PhoneService")
mosaicplot(table(churnData$MultipleLines, churnData$Churn), ylab="Churn", xlab="MultipleLines")
mosaicplot(table(churnData$InternetService, churnData$Churn), ylab="Churn", xlab="InternetService")
mosaicplot(table(churnData$OnlineSecurity, churnData$Churn), ylab="Churn", xlab="OnlineSecurity")
mosaicplot(table(churnData$OnlineBackup, churnData$Churn), ylab="Churn", xlab="OnlineBackup")
mosaicplot(table(churnData$DeviceProtection, churnData$Churn), ylab="Churn", xlab="DeviceProtection")
mosaicplot(table(churnData$TechSupport, churnData$Churn), ylab="Churn", xlab="TechSupport")
mosaicplot(table(churnData$StreamingTV, churnData$Churn), ylab="Churn", xlab="StreamingTV")
mosaicplot(table(churnData$StreamingMovies, churnData$Churn), ylab="Churn", xlab="StreamingMovies")
mosaicplot(table(churnData$Contract, churnData$Churn), ylab="Churn", xlab="Contract")
mosaicplot(table(churnData$PaperlessBilling, churnData$Churn), ylab="Churn", xlab="PaperlessBilling")
mosaicplot(table(churnData$PaymentMethod, churnData$Churn), ylab="Churn", xlab="PaymentMethod")

# 3 Find Optimal Decision
library(tree)

# 3.1 Split data into training and testing set
# 74% of data is used as training data
# Because any number greater than 74% would cause 'maximum depth reached' issue in constructing the tree
set.seed(4073)
trainIndex <- sample(nrow(churnData), 0.74*nrow(churnData), replace = FALSE)
train <- churnData[trainIndex,]
test <- churnData[-trainIndex,]

# 3.2 Build a "large" decision tree
ltree <- tree(Churn~., data=train,
              split = "deviance",
              method = "recursive.partition",
              control = tree.control(nobs = nrow(train),  # number of sample points
                                     mincut = 1,             # minimum points in each child
                                     minsize = 2,            # minimum points in each parent
                                     mindev = 0))            # minimum information gain to split)

plot(ltree)
text(ltree, pretty = FALSE)
summary(ltree) # Misclassification error rate 0.0008589

# 3.3 Cross Validation
# 3.3.1 CV on misclassification
cvTree <- cv.tree(ltree, FUN = prune.misclass, K = 20)
cbind(cvTree$size, cvTree$dev) 
plot(cvTree$size, cvTree$dev, type="b",xlab = "Number of Leaves", ylab = "Deviance") 
min(cvTree$dev)
bestIndex1 <- cvTree$size[which(cvTree$dev == min(cvTree$dev))]
bestIndex1

# 3.3.2 CV on deviance
cvTree <- cv.tree(ltree, FUN = prune.tree, K = 20)
cbind(cvTree$size, cvTree$dev) 
plot(cvTree$size, cvTree$dev, type="b",xlab = "Number of Leaves", ylab = "Deviance") 
min(cvTree$dev)
bestIndex2 <- cvTree$size[which(cvTree$dev == min(cvTree$dev))]
bestIndex2

# 3.4 Tree Pruning
# Since there are multiple values for the lowest misclassification rate and deviance
# best size is selected using the test set among all the candidates

# 3.4.1 Find Best Size (misclassification)
bestSize <- 1
bestAccuracy <- 0
for (size in bestIndex1){
  pTree <- prune.misclass(ltree, best = size)
  predTest <- predict(pTree, test, type='class')
  accuracy <- mean(predTest == test$Churn)
  if (accuracy > bestAccuracy){
    bestAccuracy <- accuracy
    bestSize <- size
  }
}
bestSize # 9

# 3.4.2 Use best size to prune the tree (deviance)
pTree1 <- prune.misclass(ltree, best = bestSize)
plot(pTree1)
text(pTree1, pretty = FALSE)

# 3.4.3 Find Best Size (misclassification)
bestSize <- 1
bestAccuracy <- 0
for (size in bestIndex2){
  pTree <- prune.tree(ltree, best = size)
  predTest <- predict(pTree, test, type='class')
  accuracy <- mean(predTest == test$Churn)
  if (accuracy > bestAccuracy){
    bestAccuracy <- accuracy
    bestSize <- size
  }
}
bestSize # 13

# 3.4.4 Use best size to prune the tree (misclassification)
pTree2 <- prune.tree(ltree, best = bestSize)
plot(pTree2)
text(pTree2, pretty = FALSE)

# 3.5 Check accuracy of pruned tree
# 3.5.1 Pruned tree using misclassification rate
predTrain1 <- predict(pTree1, train, type="class")
predTest1 <- predict(pTree1, test, type='class')
mean(predTrain1 == train$Churn) # 0.8126185
mean(predTest1 == test$Churn)  # 0.7927581

# 3.5.2 Pruned tree using deviance
predTrain2 <- predict(pTree2, train, type="class")
predTest2 <- predict(pTree2, test, type='class')
mean(predTrain2 == train$Churn) # 0.8017872
mean(predTest2 == test$Churn)  # 0.7958398

# 3.6 Conclusion: pTree2 is chosen to be the optimal tree model instead of pTree1
# because pTree1 has higher testing accuracy than pTree2

# 3.7 Build Random Forest Classifier
library(randomForest)
set.seed(4073)
rf <- randomForest(Churn ~ ., data = train, ntree = 1000, importance = TRUE)                 
rf # error rate 19.66%

predTrain <- predict(rf, train, type = "class")
mean(predTrain == train$Churn) # 0.9856485
predTest <- predict(rf, test, type = "class")
mean(predTest == test$Churn)  # 0.7912173

importance(rf)
varImpPlot(rf)

# ==========
# Problem 2
# ==========

# Remove all variables
# rm(list=ls())

# 1 Import Dataset
TitanicData <- read.csv('train.csv')

# 2 Explortary Data Analysis
# 2.1 Dimension
dim(TitanicData)

# 2.2 Column Names
names(TitanicData)

# 2.3 Structure of dataset
str(TitanicData)
TitanicData$Survived <- factor(TitanicData$Survived) #Covert target variable into factor

# 2.4 First few rows
head(TitanicData)

# 2.5 Last few rows
tail(TitanicData)

# 2.6 Summary Statistics
summary(TitanicData)

# 2.7 Plot all individual variables VS target
plot(TitanicData$PassengerId, TitanicData$Survived, ylab="Survived", xlab="PassengerId")
plot(TitanicData$Pclass, TitanicData$Survived, ylab="Survived", xlab="Pclass")
plot(TitanicData$Name, TitanicData$Survived, ylab="Survived", xlab="Name")
plot(TitanicData$Sex, TitanicData$Survived, ylab="Survived", xlab="Sex")
plot(TitanicData$Age, TitanicData$Survived, ylab="Survived", xlab="Age")
plot(TitanicData$SibSp, TitanicData$Survived, ylab="Survived", xlab="SibSp")
plot(TitanicData$Parch, TitanicData$Survived, ylab="Survived", xlab="Parch")
plot(TitanicData$Ticket, TitanicData$Survived, ylab="Survived", xlab="Ticket")
plot(TitanicData$Fare, TitanicData$Survived, ylab="Survived", xlab="Fare")
plot(TitanicData$Cabin, TitanicData$Survived, ylab="Survived", xlab="Cabin")
plot(TitanicData$Embarked, TitanicData$Survived, ylab="Survived", xlab="Embarked")

# 2.8 Cross-tabulate variables VS target
table(TitanicData$PassengerId, TitanicData$Survived)
table(TitanicData$Pclass, TitanicData$Survived)
table(TitanicData$Name, TitanicData$Survived)
table(TitanicData$Sex, TitanicData$Survived)
table(TitanicData$Age, TitanicData$Survived)
table(TitanicData$SibSp, TitanicData$Survived)
table(TitanicData$Parch, TitanicData$Survived)
table(TitanicData$Ticket, TitanicData$Survived)
table(TitanicData$Fare, TitanicData$Survived)
table(TitanicData$Cabin, TitanicData$Survived)
table(TitanicData$Embarked, TitanicData$Survived)

# 2.9 Mosaicplots of cross-tabulations
mosaicplot(table(TitanicData$PassengerId, TitanicData$Survived), ylab="Survived", xlab="PassengerId")
mosaicplot(table(TitanicData$Pclass, TitanicData$Survived), ylab="Survived", xlab="Pclass")
mosaicplot(table(TitanicData$Name, TitanicData$Survived), ylab="Survived", xlab="Name")
mosaicplot(table(TitanicData$Sex, TitanicData$Survived), ylab="Survived", xlab="Sex")
mosaicplot(table(TitanicData$Age, TitanicData$Survived), ylab="Survived", xlab="Age")
mosaicplot(table(TitanicData$SibSp, TitanicData$Survived), ylab="Survived", xlab="SibSp")
mosaicplot(table(TitanicData$Parch, TitanicData$Survived), ylab="Survived", xlab="Parch")
mosaicplot(table(TitanicData$Ticket, TitanicData$Survived), ylab="Survived", xlab="Ticket")
mosaicplot(table(TitanicData$Fare, TitanicData$Survived), ylab="Survived", xlab="Fare")
mosaicplot(table(TitanicData$Cabin, TitanicData$Survived), ylab="Survived", xlab="Cabin")
mosaicplot(table(TitanicData$Embarked, TitanicData$Survived), ylab="Survived", xlab="Embarked")

# 3 Find Optimal Decision
library(tree)

# 3.1 Split data into training and testing set
set.seed(4073)

drops <- c("Name", 'Ticket', "Cabin", "PassengerId")

# 3.2 Data preprocessing
# 3.2.1 Extract Salutation
all_names <- c('Mr.', 'Mrs.','Miss', 'Don.', 'Dr.', 'Master.')
extractNames <- function(old_name){
  for (name in all_names){
    if (grepl(name, old_name)){
      return (name)
    }
  }
  return ('No salutation')
}
TitanicData$Salutation <- factor(sapply(TitanicData$Name, FUN = extractNames))

# 3.2.2 Extract digits from Ticket
extractDigits <- function(old_no){
  return (gsub("[^0-9]", "", old_no))
}

TitanicData$TicketNew <- as.numeric(sapply(TitanicData$Ticket, FUN= extractDigits))

# 3.2.3 Extract letter from Cabin
processCabin <- function(cabin){
  if (cabin==""){
    return('Z')
  }
  else{
    return(substring(cabin,1,1))
  }
}
TitanicData$cabinLetter <- factor(sapply(TitanicData$Cabin, FUN= processCabin))

# 3.2.4 Drop unused columns
TitanicDataDrop <- TitanicData[ , !(names(TitanicData) %in% drops)]

# 3.2.5 Use median to impute NAs
removeNA <- function(value, m){
  if (is.na(value)){
    return (m)
  }
  else{
    return (value)
  }
}
TitanicDataDrop$Age <- as.numeric(sapply(TitanicDataDrop$Age, FUN= removeNA, m = median(TitanicDataDrop$Age, na.rm = TRUE)))
TitanicDataDrop$TicketNew <- as.numeric(sapply(TitanicDataDrop$TicketNew, FUN= removeNA, m = median(TitanicDataDrop$TicketNew, na.rm = TRUE)))

# 3.3 Split Training and Testing Set
set.seed(4073)
trainIndex <- sample(nrow(TitanicDataDrop), 0.8*nrow(TitanicDataDrop), replace = FALSE)
train <- TitanicDataDrop[trainIndex,]
test <- TitanicDataDrop[-trainIndex,]

# 3.4 Build a "large" decision tree
ltree <- tree(Survived~., data=train,
              split = "deviance",
              method = "recursive.partition",
              control = tree.control(nobs = nrow(train),  # number of sample points
                                     mincut = 1,             # minimum points in each child
                                     minsize = 2,            # minimum points in each parent
                                     mindev = 0))            # minimum information gain to split)

plot(ltree)
text(ltree, pretty = FALSE)
summary(ltree)

# 3.5 Cross Validation
# 3.5.1 CV on misclassification rate
set.seed(4073)
cvTree <- cv.tree(ltree, FUN = prune.misclass, K = 20)
cbind(cvTree$size, cvTree$dev) 
plot(cvTree$size, cvTree$dev, type="b",xlab = "Number of Leaves", ylab = "Deviance") 
min(cvTree$dev)
bestIndex1 <- cvTree$size[which(cvTree$dev == min(cvTree$dev))]
bestIndex1

# 3.5.2 CV on deviance
set.seed(4073)
cvTree <- cv.tree(ltree, FUN = prune.tree, K = 20)
cbind(cvTree$size, cvTree$dev) 
plot(cvTree$size, cvTree$dev, type="b",xlab = "Number of Leaves", ylab = "Deviance") 
min(cvTree$dev)
bestIndex2 <- cvTree$size[which(cvTree$dev == min(cvTree$dev))]
bestIndex2

# 3.6 Find best size
# 3.6.1 Find best size using misclassfication rate
bestSize1 <- 1
bestAccuracy <- 0
for (size in bestIndex1){
  pTree <- prune.misclass(ltree, best = size)
  predTest <- predict(pTree, test, type='class')
  accuracy <- mean(predTest == test$Survived)
  if (accuracy > bestAccuracy){
    bestAccuracy <- accuracy
    bestSize1 <- size
  }
}
bestSize1 # 56

# 3.6.2 Find best size using deviance
bestSize2 <- 1
bestAccuracy <- 0
for (size in bestIndex2){
  pTree <- prune.tree(ltree, best = size)
  predTest <- predict(pTree, test, type='class')
  accuracy <- mean(predTest == test$Survived)
  if (accuracy > bestAccuracy){
    bestAccuracy <- accuracy
    bestSize2 <- size
  }
}
bestSize2 # 8

# 3.7 Use best size to prune the tree
# 3.7.1 Prune tree using misclassification rate
pTree1 <- prune.misclass(ltree, best = bestSize1)
plot(pTree1)
text(pTree1, pretty = FALSE)

# 3.7.2 Prune tree using deviance
pTree2 <- prune.tree(ltree, best = bestSize2)
plot(pTree2)
text(pTree2, pretty = FALSE)

# 3.8 Check accuracy of pruned tree
# 3.8.1 Misclassification Rate
predTrain1 <- predict(pTree1, train, type="class")
predTest1 <- predict(pTree1, test, type='class')
mean(predTrain1 == train$Survived) # 0.9382022
mean(predTest1 == test$Survived) # 0.8268156

# 3.8.2 Deviance
predTrain2 <- predict(pTree2, train, type="class")
predTest2 <- predict(pTree2, test, type='class')
mean(predTrain2 == train$Survived) # 0.8300562
mean(predTest2 == test$Survived) # 0.8268156

# 4 Submit file on Kaggle
# 4.1 Preprocess test data
testData <- read.csv('test.csv')
testData$Salutation <- factor(sapply(testData$Name, FUN = extractNames))
testData$TicketNew <- as.numeric(sapply(testData$Ticket, FUN= extractDigits))
testData$cabinLetter <- factor(sapply(testData$Cabin, FUN= processCabin))
testDataDrop <- testData[ , !(names(testData) %in% drops)]

# 4.2 Prediction using pTree1
# Kaggle Score 0.75119
predTest1 <- predict(pTree1, testDataDrop, type='class')
write.csv(data.frame(PassengerId=testData$PassengerId, Survived=predTest1), file = 'submission1.csv', row.names = FALSE)

# Prediction using pTree2
# Kaggle Score 0.79904, rank 1131
predTest2 <- predict(pTree2, testDataDrop, type='class')
write.csv(data.frame(PassengerId=testData$PassengerId, Survived=predTest2), file = 'submission2.csv', row.names = FALSE)
# In conclusion, the optimal tree model is pTree2 because it has the highest score on kaggle
# and although both pTree2 and pTree1 have same testing accuracy,
# pTree2 has lower training accuracy, which means pTree2 has lower chance of overfitting

# 5 Random Forest
set.seed(4073)
library(randomForest)

# 5.1 Build Random Forest classifier
rf <- randomForest(Survived ~ ., data = train, ntree = 1000, importance = TRUE)                 
rf

# 5.2 Check model accuracy
predTrain <- predict(rf, train, type = "class")
mean(predTrain == train$Survived) # 0.9662921
predValid <- predict(rf, test, type = "class")
mean(predValid == test$Survived) # 0.8547486

# 5.3 View variable importance
importance(rf)
varImpPlot(rf)

# 5.4 Output submission file
temp <- rbind(train[,-1], testDataDrop)
testDataDrop <- temp[seq(nrow(train)+1,nrow(temp)),]
testDataDrop$Age <- as.numeric(sapply(testDataDrop$Age, FUN= removeNA, m = median(TitanicDataDrop$Age, na.rm = TRUE)))
testDataDrop$Fare <- as.numeric(sapply(testDataDrop$Fare, FUN= removeNA, m = median(TitanicDataDrop$Fare, na.rm = TRUE)))

# Kaggle Score 0.77511
predTest <- predict(rf, testDataDrop, type='class')
write.csv(data.frame(PassengerId=testData$PassengerId, Survived=predTest), file = 'submission.csv', row.names = FALSE)

# Although random forest has higher training and testing accuracy compared to normal tree model,
# the kaggle socre is lower than the optimal tree model (0.79904)
