#Install and call all libraries ----------------------------------
library(kernlab)
library(caret)
library(tm)
library(SnowballC)
library(wordcloud)
library(quanteda)
library(klaR)
library(MASS)
library(e1071)
library(party)

set.seed(100)

list.of.packages <- c("Matrix", "SnowballC", "wordcloud", "quanteda", "e1071", "klaR", "ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#listing all the variables used ------------------------------------
# Create dataframe y, x1, x2, x3, x4, x5...
ss <- German
df1 <- as.data.frame(ss)
df1$Status_of_existing_checking_acount = factor(df1$`Status of existing checking acount`)

df1$`Status of existing checking acount` = NULL

df1$Credit_history = factor(df1$`Credit history`)

df1$`Credit history` = NULL

df1$Purpose=factor(df1$Purpose)
df1$Savings_account_bonds = factor(df1$`Savings account/bonds`)
df1$`Savings account/bonds`= NULL
df1$Present_Employment =factor(df1$`Present Employment`)
df1$`Present Employment`=NULL
df1$Personal_status_and_sex =factor(df1$`Personal status and sex `)
df1$`Personal status and sex `=NULL
df1$Other_debtors_guarantors = factor(df1$`Other debtors / guarantors `)
df1$`Other debtors / guarantors `=NULL
df1$Property=factor(df1$Property)
df1$Other_installment_plans =factor(df1$`Other installment plans`)
df1$`Other installment plans`=NULL
df1$Housing = factor(df1$Housing)
df1$Job=factor(df1$Job)
df1$Telephone=factor(df1$Telephone)
df1$foreign_worker =factor(df1$`foreign worker `)
df1$`foreign worker `=NULL


df1$Default=factor(df1$Default)
colnames(df1)

#df1 = df1[,c(22,2, 23, 4,5, 24, 25, 8, 26, 27,11,12, 13, 28, 15,16, 17,18,19,29,21 )]

df1 = df1[,c(-16,-10,-6,-5)]
df_1 <- as.vector(ss)
n1 = nrow(df1)

#selecting which variables
df1.cont=df1[,c(2,5,8,11,13,16)]
pairs(df1.cont)

for(i in 1:ncol(df1)){
  
  
  if(i != 13) {
    
    plot(df1[,i],df1$Default, xlab = colnames(df1)[i])
  }
}

#removing the unwanted variables
df1 = df1[,c(-1,-3,-4,-5,-7,-9,-10,-11,-12,-17,-18,-19)]

library(ggplot2)   
# logistic regression --------------------------------------------

glm.model <- train(as.factor(Default) ~ ., 
                   data = df1, 
                   method = "glm", family = binomial(link = "logit"),
                   trControl = trainControl(method = "cv", number = n1-1))




summary(glm.model)
fitted(glm.model)

# Important variables according to glm model

glm.imp <- varImp(glm.model, scale = FALSE)
print(glm.imp)
plot(glm.imp, main = "Logistic Regression Important features")

#pairs(df1)

# predicted default type
glm.pred <- predict(glm.model, df1, type = "raw")

# Error metrics
glm.metrics <- table(glm.pred, ss$Default)
print(glm.metrics)

# SVM -------------------------------------------------------------------------
set.seed(100)

# Training svm model
svm.model <- train(as.factor(Default) ~., 
                   data = df1, 
                   method = "svmLinear", preProcess = c("center", "scale"),
                   trControl = trainControl(method = "cv", number = n1-1))
print(svm.model)
# Important variables according to SVM model
svm.imp <- varImp(svm.model)
print(svm.imp)
plot(svm.imp, main = "SVM Important features")

# prediction on test data 
svm.pred <- predict(svm.model, df1)

# error metrics
svm.metrics <- table(svm.pred, df1$Default)
print(svm.metrics)


# Neural networks--------------------------------------------------------------------------

set.seed(100)

#will take a few minutes
nnet.model <- train(as.factor(Default) ~.,  
                    data = df1, 
                    trControl = trainControl(method = "cv", number = 50),
                    method='pcaNNet')
print(nnet.model)
# Important variables according to ctree model
nnet.imp <- varImp(nnet.model)
print(nnet.imp)
plot(nnet.imp, main = "Neural Network Important features")

# prediction on test data
nnet.pred <- predict(nnet.model, df1, type = "raw")

# Error metrics
nnet.metrics <- table(nnet.pred, df1$Y)
print(nnet.metrics)

#Decision Tree --------------------------------------------

set.seed(100)

tree.model <- train(as.factor(Default) ~., 
                    data = df1, 
                    trControl = trainControl(method = "cv", number = n1-1),
                    method = "ctree")
sal <- ctree(df1$Default~.,data = df1, controls=ctree_control(testtype="Teststatistic"))
print(tree.model)

# Important variables according to ctree model
tree.imp <- varImp(tree.model)
print(tree.imp)
plot(tree.imp, main = "Decision Tree Important features")

# prediction on test data
tree.pred <- predict(tree.model, df1, type = "raw")

# Error metrics
tree.metrics <- table(tree.pred, df1$Default)
print(tree.metrics)

