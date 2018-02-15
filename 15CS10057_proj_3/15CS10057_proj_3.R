sourceDir <- getSrcDirectory(function(dummy) {dummy})
setwd(sourceDir)
data<-read.csv("Customer Churn Data.csv")
library(caret)
library(klaR)
library(e1071)
library(rpart.plot)

#for installing the package caret: install.packages('caret', dependencies = TRUE)


# data pruning and validation
data$area_code=factor(data$area_code)
data$phone_number=NULL
# creating partition on basis of the churn data - having same proportion of the values in each column
trainIndex <- createDataPartition(data$churn, p = .8, list = FALSE, times = 1)   

#for plotting account_lenght: ggplot(data, aes(x=account_length, fill=churn))+geom_density()+ facet_grid(churn ~ .) + labs(title="Account Length")

data$area_code=factor(data$area_code)
data$phone_number=NULL                                   # omitting the attribute altogether
#splitting the data evenly on the basis of the churn column. We now have a train and test partition, 
#each having the same proportion of True and False values for the "churn" attribute
dataTrain <- data[ trainIndex,]
dataTest  <- data[-trainIndex,]

# dividing into attribute and classifcation for test and train data
dataTrain_x=dataTrain[,2:20]                     # taking all columns except first(Id) and last(churn), which defines the class
dataTrain_y=dataTrain$churn

dataTest_x=dataTest[,2:20]
dataTest_y=dataTest$churn

# The NAIVE BAYES model
# this model, in its simplest form has no additional parameters and hence is quite easy and straight-forward to use
nb_model=naiveBayes(dataTrain_x,dataTrain_y,laplace =1)          # of package e1071. The NaiveBayes clf of klaR package has additional features, but something
pred_nb=predict(nb_model,dataTest_x)                              #... causes it to throw error in prediction phase. This method gives similar results to that one though.
cm_nb=confusionMatrix(pred_nb,dataTest_y)
print(nb_model$tables)

# The Decision Tree Classifier 
dec_tree=rpart(churn~.,data=dataTrain[2:21],method="class",parms = list(split = "information"))#dataTrain$state+dataTrain$account_length+dataTrain$area_code
pred_tree=predict(dec_tree,dataTest_x,type="class")
cm_tree=confusionMatrix(pred_tree,dataTest_y)
rpart.plot(dec_tree)

# The Support Vector Machine classfier
# To plot data:  plot(model_svm,data=dataTrain[2:21],formula=total_day_calls~number_customer_service_calls)
#model_svm=svm(dataTrain_y,x=dataTrain$total_day_calls+dataTrain$total_day_minutes+dataTrain$total_day_charge+dataTrain$number_customer_service_calls,kernel="linear")

model_svm=svm(churn~.,data=(dataTrain[2:21]),cost=5,kernel="linear",scale=FALSE)
pred_svm=predict(model_svm,dataTest_x)
cm_svm=confusionMatrix(pred_svm,dataTest_y)

# polynomial SVM:
model_svm_poly=tune.svm(churn~.,data=(dataTrain[2:21]),cost=5,kernel="polynomial",degree=c(2,3,4),gamma=c(0.01,0.1,1))#,coef0 = c(0,1,2,3,4),gamma=c(0.01,0.1,1,2,5)
pred_svm_poly=predict(model_svm_poly$best.model,dataTest_x)
cm_svm_poly=confusionMatrix(pred_svm_poly,dataTest_y)
plot(model_svm_poly$best.model,data=dataTrain[2:21],formula=total_day_calls~number_customer_service_calls)

cat("Naive Bayes:")
print(cm_nb)
cat("\nDecision Tree:")
print(cm_tree)
cat("\nLinear SVM:")
print( cm_svm)
cat("\n Polynomial SVM (tuned):")
print(cm_svm_poly)