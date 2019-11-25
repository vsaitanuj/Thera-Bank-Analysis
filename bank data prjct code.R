######################### THERA BANK ANALYSIS #############################

#### Importing the necessary libraries #####
install.packages("readxl")
library(readxl)
install.packages("ggplot2")
library(ggplot2)
install.packages("Hmisc")
library(Hmisc)
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("randomForest")
library(randomForest)
install.packages("caTools")
library(caTools)
install.packages("psych")
library(psych)
install.packages("ROCR")
library(ROCR)
install.packages("InformationValue")
library(InformationValue)
install.packages("ineq")
library(ineq)
install.packages("memisc")
library(memisc)
install.packages("cluster")
library(cluster)
install.packages("NbClust")
library(NbClust)
#### Changing the working directory #####
setwd("C:/R programs great lakes/DATA MINING/prjct")
getwd()

#### Importing the dataset "THERA BANK" dataset #####
bank_data = read_xlsx("Bank_data.xlsx")
bank_data

#### Performing EDA on the dataset #####
dim(bank_data)
str(bank_data)
head(bank_data)
tail(bank_data)
summary(bank_data)
colnames(bank_data)

## Converting the variables into proper Columnnames #####
colnames(bank_data) = make.names(colnames(bank_data))
colnames(bank_data)

## Changing the negative values in the Experience Column to zeros ####
sum(bank_data$Experience..in.years. < 0)
bank_data[bank_data < 0] = 0
sum(bank_data$Experience..in.years. < 0)
## Fixing the Pincodes ####
pincode = bank_data
sum(pincode$ZIP.Code < 10000)
which(pincode$ZIP.Code < 10000)
which(colnames(pincode) == "ZIP.Code")
pincode[385,5] = pincode[385,5] * 10 
pincode[385,5] = pincode[385,5] + 5
pincode[385,5]
sum(pincode$ZIP.Code < 10000)

#### Uni - Variate Analysis #####
### Converting the requiered variables into Categorical variable ###
bank_data$Personal.Loan = factor(bank_data$Personal.Loan,levels = c(1,0),labels = c("Yes","No"))
bank_data$Education = factor(bank_data$Education,levels = c(1,2,3),labels = c("Undergrad","Graduates","Proffesional"))
bank_data$Securities.Account = factor(bank_data$Securities.Account,levels = c(1,0),labels = c("Yes","No"))
bank_data$CD.Account = factor(bank_data$CD.Account,levels = c(1,0),labels = c("Yes","No"))
bank_data$Online = factor(bank_data$Online,levels = c(1,0),labels = c("Yes","No"))
bank_data$CreditCard = factor(bank_data$CreditCard,levels = c(1,0),labels = c("Yes","No"))
bank_data$Family.members = as.factor(bank_data$Family.members)
## Working with the response variable #####
print(table(bank_data$Personal.Loan))
print(prop.table(table(bank_data$Personal.Loan))*100)
plot(bank_data$Personal.Loan,main = "Personal Loan")
## Working with other categorical variables ##
print(table(bank_data$Family.members))
print(prop.table(table(bank_data$Family.members))*100)
plot(bank_data$Family.members,main = "Family Members")
# Education #
print(table(bank_data$Education))
print(prop.table(table(bank_data$Education))*100)
plot(bank_data$Education,main = "Education")
# Securities Account #
print(table(bank_data$Securities.Account))
print(prop.table(table(bank_data$Securities.Account))*100)
plot(bank_data$Securities.Account,main = "Securities Account")
# Certificate of Deposit #
print(table(bank_data$CD.Account))
print(prop.table(table(bank_data$CD.Account))*100)
plot(bank_data$CD.Account,main = "Certificate of Deposit")
# Internet Banking #
print(table(bank_data$Online))
print(prop.table(table(bank_data$Online))*100)
plot(bank_data$Online,main = "Internet Banking")
# Credit Card issued by the bank #
print(table(bank_data$CreditCard))
print(prop.table(table(bank_data$CreditCard))*100)
plot(bank_data$CD.Account,main = "Credit issued by the bank")
# Plotting all the numerical variables #
hist.data.frame(bank_data[,c(2,3,4,7,9)])
plot(bank_data[,c(2,3,4,7,9)])


##### Bi-Variate Analysis #####
### Response variable with Categorical variable ###
table(bank_data$Personal.Loan,bank_data$Family.members)
qplot(Personal.Loan,fill = Family.members,data = bank_data) 
table(bank_data$Personal.Loan,bank_data$CreditCard)
qplot(Personal.Loan,fill = CreditCard,data = bank_data) 

### Independent variables with Independent variables ###
qplot(Family.members,fill = Education,data = bank_data)
qplot(CreditCard,fill = Online,data = bank_data)

### Dependent variables with Numerical variables ###
qplot(Income..in.K.month.,fill = Personal.Loan,data = bank_data)
qplot(Mortgage,fill = Personal.Loan,data = bank_data,geom = "dotplot")
qplot(CCAvg,fill = Personal.Loan,data = bank_data,geom = "density")

### Other Dependent variables with the Numerical Variables ###
qplot(Income..in.K.month.,fill = CreditCard,data = bank_data)
qplot(Mortgage,fill = CreditCard,data = bank_data)

## Identifying and Changing the NA's from the family Column #####
bank_data$Family.members = as.numeric(bank_data$Family.members)
sum(is.na(bank_data))
bank_data[is.na(bank_data)] = 0
sum(is.na(bank_data))
bank_data$Family.members = as.factor(bank_data$Family.members)

#### Finding the Outliers ####
boxplot(bank_data[,c(2,3,4,7,9)])

#### Removing the ID variable and ZIP.CODES variable ####
bank_data = bank_data[,-c(1,5)]
colnames(bank_data)

#### Clustering Analysis ####
### Hierarchial Clustering ###
scale_bank_data = scale(bank_data[,c(3,5,7)])
## Taking the distance matrix ##
dist_bank_data = dist(scale_bank_data)
## Creating Hierarchial Clusters ####
hclust_bank_data = hclust(dist_bank_data,method = "complete")
plot(hclust_bank_data)
rect.hclust(hclust_bank_data,k = 3)
### Clustering the dataset using the Cluster object created ###
bank_data_hclust = bank_data[,c(3,5,7)]
bank_data_hclust$cluster = cutree(hclust_bank_data,k = 3)
bank_data_hclust = aggregate(bank_data_hclust,list(bank_data_hclust$cluster),FUN = "mean")
print(bank_data_hclust)

### K-Means Clustering  ###
#### Randomly Clustering with the infered number of clusters ####
set.seed(77)
library(cluster)
k.cluster = kmeans(dist_bank_data,centers = 3,nstart = 5)
clusplot(bank_data,k.cluster$cluster,lines = 1,color = TRUE,shade = TRUE)


### Trying to find the right number of Clusters by calculating variance ###
set.seed(77)
k.values = c(1:10)
kcluster2 = kmeans(dist_bank_data,centers = 9,nstart = 13)
plot(k.values, kcluster2$withinss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

### K means Clustering using the number of clusters obtained after the experiment ###
set.seed(77)
nc1 = NbClust(bank_data[,c(3,5,7)],min.nc = 2,max.nc = 5,method = "kmeans")
### Perfroming the K means Clustering with number of clusters as 4 ####
set.seed(77)
kcluster = kmeans(dist_bank_data,centers = 4,nstart = 8)
clusplot(bank_data,kcluster$cluster,lines = 1,color = TRUE,shade = TRUE)
### Using the clustering object on the dataset ####
bank_data_kclust = bank_data[,c(3,5,7)]
bank_data_kclust$Cluster = kcluster$cluster
bank_data_kclust = aggregate(bank_data_kclust,list(bank_data_kclust$Cluster),FUN = mean)
print(bank_data_kclust)

#### CREATING DECISION TREES #####
### Splitting the dataset into training and testing dataset ####
####(Industry standard splitting ratio of 0.7) ####
set.seed(77)
split <- sample.split(bank_data$Personal.Loan, SplitRatio = 0.7)
train<- subset(bank_data, split == TRUE)
test<- subset(bank_data, split == FALSE)
dim(train)
dim(test)

####CART MODEL ####
### For the process of creating a CART Basesd Model on train data and using it on train data, ###
### we create a dataframe named  "tntrain1" similar to the original train dataset ###
tntrain1 = train
tntrain1$Personal.Loan = factor(tntrain1$Personal.Loan,levels = c("Yes","No"),labels = c(1,0))
### Creating a CART model based on train data ###
set.seed(77)
rttree = rpart(tntrain1$Personal.Loan~.,data = tntrain1,minsplit = 10,minbucket = 10)
print(rttree)
rpart.plot(rttree)
### Since we can see that the tree is complex, we need find the decide the right Complexity Parameter ###
### to prune the tree in the right way ###
printcp(rttree)
plotcp(rttree)
### From the CP graph and the CP table, we can see that, the right CP is 0.025298 #####
### Pruning the tree using the CP, 0.025298 ####
prttree = prttree = prune(rttree,cp = 0.025298)
print(prttree)
rpart.plot(prttree)
### Using the model to make predictions on the tntrain1 dataset ####
tntrain1$predict = predict(prttree,tntrain1,type = "class")
tntrain1$prob = predict(prttree,tntrain1,type = "prob")[,1]
View((head(tntrain1)))

##### Testing the Performance of the model on the tntrain1 dataset ####
## CONCORDANCE RATIO (TRAINING MODEL ON TRAINING DATA) ##
x = tntrain1$Personal.Loan
y = tntrain1$prob
Concordance(actuals = x,predictedScores = y)
## CONFUSION MATRIX ERROR RATE (TRAINING MODEL ON TRAINING DATA) ## 
cnf_tntrain1 = table(tntrain1$Personal.Loan,tntrain1$predict)
errrate_tntrain1 = (cnf_tntrain1[2,1]+cnf_tntrain1[1,2])/nrow(tntrain1)
print(errrate_tntrain1)
## PLOTTING THE TPR AND FPR GRAPH AND FINDING ROC ###
##(TRAINING MODEL ON TRAINING DATA) ##
tntrain1.pred.obj = prediction(tntrain1$prob,tntrain1$Personal.Loan)
tntrain1.perf = performance(tntrain1.pred.obj,"tpr","fpr")
plot(tntrain1.perf)
### KS VALUE ###
print(max(tntrain1.perf@y.values[[1]] - tntrain1.perf@x.values[[1]]))
#### AUC (TRAINING MODEL ON TRAINING DATA) ####
tntrain1.auc = performance(tntrain1.pred.obj,"auc")
tntrain1.auc = as.numeric(tntrain1.auc@y.values)
print(tntrain1.auc)
#### GINI COEFFICIENT (TRAINING MODEL ON TRAINING DATA) ####
gini1 = ineq(tntrain1$prob,"gini")
print(gini1)

### For the process of testing the CART Based Model on train data and using it on test data, ###
### we create a dataframe named  "tntest1" similar to the original test dataset ###
tntest1 = test
tntest1$Personal.Loan = factor(tntest1$Personal.Loan,levels = c("Yes","No"),labels = c(1,0))
### Using the training data CART model to make predicitions on the test data ###
tntest1$predict = predict(prttree,tntest1,type = "class")
tntest1$prob = predict(prttree,tntest1,type = "prob")[,1]
View((head(tntest1)))
## CONCORDANCE RATIO (TRAINING MODEL ON TESTING DATA) ##
a = tntest1$Personal.Loan
b = tntest1$prob
Concordance(actuals = a,predictedScores = b)
## CONFUSION MATRIX ERROR RATE (TRAINING MODEL ON TESTING DATA) ## 
cnf_tntest1 = table(tntest1$Personal.Loan,tntest1$predict)
errrate_tntest1 = (cnf_tntest1[2,1]+cnf_tntest1[1,2])/nrow(tntest1)
print(errrate_tntest1)
## PLOTTING THE TPR AND FPR GRAPH TO FIND ROC (TRAINING MODEL ON TESTING DATA) ##
tntest1.pred.obj = prediction(tntest1$prob,tntest1$Personal.Loan)
tntest1.perf = performance(tntest1.pred.obj,"tpr","fpr")
plot(tntest1.perf)
#### KS VALUE (TRAINING MODEL ON TESTING DATA) ####
print(max(tntest1.perf@y.values[[1]] - tntest1.perf@x.values[[1]]))
#### AUC (TRAINING MODEL ON TESTING DATA) ####
tntest1.auc = performance(tntest1.pred.obj,"auc")
tntest1.auc = as.numeric(tntest1.auc@y.values)
print(tntest1.auc)
#### GINI INDEX (TRAINING MODEL ON TESTING DATA)####
gini2 = ineq(tntest1$prob,"gini")
print(gini2)

### For the process of creating a Random Forest Model on train data and using it on train data, ###
### we create a dataframe named  "tntrain2" similar to the original train dataset ###
tntrain2 = train
tntrain2$Personal.Loan = factor(tntrain2$Personal.Loan,levels = c("Yes","No"),labels = c(1,0))
### Creating a Random Forest model on the train data ###
#### Building a random forest using the random number of trees for the random forest ####
set.seed(77)
tntrain2.rf = randomForest(tntrain2$Personal.Loan~.,data = tntrain2,
                           ntree =501,mtry = 3,nodesize = 10,importance = TRUE)
plot(tntrain2.rf)
print(tntrain2.rf)
error = tntrain2.rf$err.rate
error = as.data.frame(error)
error$ntree = c(1:nrow(error))
s.error = error[order(error$OOB,error$ntree),]
s.error$ntree[1]
### From the above, we can determine that the right number of trees is 57 ####
### Finidng the right number of mtry ###
set.seed(77)
tuned.tntrain2.rf = tuneRF(x = tntrain2[,-c(8)],y = tntrain2$Personal.Loan,mtryStart = 3,stepFactor=1.5,
                           nodesize = 10,ntreeTry = 57,importance = TRUE, 
                           improve = 0.01,trace = TRUE,plot = TRUE,doBest = TRUE)
print(tuned.tntrain2.rf)

### Using the tuned Random Forest on train data to make predictions ####
tntrain2$predict = predict(tntrain2.rf,tntrain2,type = "class")
tntrain2$prob = predict(tntrain2.rf,tntrain2,type = "prob")[,"1"]
View(tntrain2)

### Testing the Random Forest on train data ####
## CONCORDANCE RATIO (TRAINING RANDOM FOREST ON TRAINING DATA) ##
c = tntrain2$Personal.Loan
d = tntrain2$prob
Concordance(actuals = c,predictedScores = d)
## CONFUSION MATRIX ERROR RATE (TRAINING RANDOM FOREST ON TRAINING DATA) ## 
cnf_tntrain2 = table(tntrain2$Personal.Loan,tntrain2$predict)
errrate_tntrain2 = (cnf_tntrain2[2,1]+cnf_tntrain2[1,2])/nrow(tntrain2)
print(errrate_tntrain2)
## PLOTTING THE TPR AND FPR GRAPH TO FIND ROC (TRAINING RANDOM FOREST ON TRAINING DATA) ##
tntrain2.pred.obj = prediction(tntrain2$prob,tntrain2$Personal.Loan)
tntrain2.perf = performance(tntrain2.pred.obj,"tpr","fpr")
plot(tntrain2.perf)
## KS VALUE (TRAINING RANDOM FOREST ON TRAINING DATA) ##
print(max(tntrain2.perf@y.values[[1]] - tntrain2.perf@x.values[[1]]))
#### AUC (TRAINING RANDOM FOREST ON TRAINING DATA) ####
tntrain2.auc = performance(tntrain2.pred.obj,"auc")
tntrain2.auc = as.numeric(tntrain2.auc@y.values)
print(tntrain2.auc)
#### Gini Coefficient (TRAINING RANDOM FOREST ON TRAINING DATA) ####
gini3 = ineq(tntrain2$prob,"gini")
print(gini3)

### For the process of testing the Random Forest Model on test data and using it on test data, ###
### we create a dataframe named  "tntest2" similar to the original test dataset ###
tntest2 = test
tntest2$Personal.Loan = factor(tntest2$Personal.Loan,levels = c("Yes","No"),labels = c(1,0))

### Using the tuned Random Forest on test data to make predictions ####
tntest2$predict = predict(tntrain2.rf,tntest2,type = "class")
tntest2$prob = predict(tntrain2.rf,tntest2,type = "prob")[,"1"]
View(tntest2)

### Testing the Random Forest on test data ####
## CONCORDANCE RATIO (TRAINING RANDOM FOREST ON TESTING DATA) ##
c = tntest2$Personal.Loan
d = tntest2$prob
Concordance(actuals = c,predictedScores = d)
## CONFUSION MATRIX ERROR RATE (TRAINING RANDOM FOREST ON TESTING DATA) ## 
cnf_tntest2 = table(tntest2$Personal.Loan,tntest2$predict)
errrate_tntest2 = (cnf_tntest2[2,1]+cnf_tntest2[1,2])/nrow(tntest2)
print(errrate_tntest2)
## PLOTTING THE TPR AND FPR GRAPH AND FINDING ROC(TRIAINING RANDOM FOREST ON TESTING DATA) ##
tntest2.pred.obj = prediction(tntest2$prob,tntest2$Personal.Loan)
tntest2.perf = performance(tntest2.pred.obj,"tpr","fpr")
plot(tntest2.perf)
## KS VALUE (TRIAINING RANDOM FOREST ON TESTING DATA) ##
print(max(tntest2.perf@y.values[[1]] - tntest2.perf@x.values[[1]]))
#### AUC (TRAINING RANDOM FOREST ON TESTING DATA) ####
tntest2.auc = performance(tntest2.pred.obj,"auc")
tntest2.auc = as.numeric(tntest2.auc@y.values)
print(tntest2.auc)
#### GINI (TRAINING RANDOM FOREST ON TESTING DATA) ####
gini4 = ineq(tntest2$prob,"gini")
print(gini4)






