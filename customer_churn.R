###########################################
# Churn Analytics using R                 #
# Poo Kuan Hoong                          #
# Malaysia R User Group Meetup            #  
# http://www.github.com/kuanhoong/churn-r #
###########################################

setwd('C:/Users/Kuan/Documents/github/churn-r')

library(caret)
library(ggplot2)
library(data.table)
library(car)
library(corrplot)
library(rattle)
library(randomForest)
library(C50)
library(rpart)
library(ROCR)
library(e1071)
library(gmodels)

#load the data
library(data.table)
cust_data<-fread('telco.csv', header=TRUE, sep=",")

#########################
# Data Proprocessing    #        
#########################

# Remove Unwanted Variables
cust_data <- cust_data[,-1]

# Handling Missing Values: Replace NAs as 0
cust_data[is.na(cust_data)] <- 0

# Recode Variables: Replace Churn status, Yes = 1, No = 1
cust_data$Churn <-replace(cust_data$Churn,cust_data$Churn == "No",0)
cust_data$Churn <-replace(cust_data$Churn,cust_data$Churn == "Yes",1)
cust_data$Churn<-as.numeric(cust_data$Churn)


# Recode Variables: Recode using the library(car) package
cust_data$gender<-recode(cust_data$gender, "'Male'=1; 'Female'=0")
cust_data$Partner<-recode(cust_data$Partner, "'Yes'=1; 'No'=0")
cust_data$Dependents<-recode(cust_data$Dependents, "'Yes'=1; 'No'=0")
cust_data$PhoneService <- recode(cust_data$PhoneService, "'Yes'=1; 'No'=0")
cust_data$MultipleLines <- recode(cust_data$MultipleLines, "'Yes'=1; 'No'=0;'No phone service'=3")
cust_data$InternetService <- recode(cust_data$InternetService, "'No'=0; 'DSL'=1;'Fiber optic'=2")
cust_data$OnlineSecurity <- recode(cust_data$OnlineSecurity, "'No'=0; 'Yes'=1;'No internet service'=2")
cust_data$OnlineBackup <- recode(cust_data$OnlineBackup, "'No'=0; 'Yes'=1;'No internet service'=2")
cust_data$DeviceProtection <- recode(cust_data$DeviceProtection, "'No'=0; 'Yes'=1;'No internet service'=2")
cust_data$TechSupport <- recode(cust_data$TechSupport, "'No'=0; 'Yes'=1;'No internet service'=2")
cust_data$StreamingTV <- recode(cust_data$StreamingTV, "'No'=0; 'Yes'=1;'No internet service'=2")
cust_data$StreamingMovies <- recode(cust_data$StreamingMovies, "'No'=0; 'Yes'=1;'No internet service'=2")
cust_data$Contract <- recode(cust_data$Contract, "'Month-to-month'=0; 'One year'=1;'Two year'=2")
cust_data$PaperlessBilling<- recode(cust_data$PaperlessBilling, "'Yes'=1; 'No'=0")
cust_data$PaymentMethod <- recode(cust_data$PaymentMethod, "'Electronic check'=1; 'Mailed check'=2;'Bank transfer (automatic)'=3; 'Credit card (automatic)'=4")

#convert column to factor
cust_data[,'Churn'] <- lapply(cust_data[,'Churn'] , factor)

#################################
# Data Exploratory              #
#################################

# overview of customer data
View(cust_data)
summary(cust_data)
str(cust_data)

# Correlation Matrix
corrmatrix <- round(cor(cust_data[,-'Churn']), digits = 2)
corrmatrix

# heatmap of correlation matrix using ggplot2
qplot(x=Var1, y=Var2, data=melt(cor(cust_data[,-'Churn'], use="p")), fill=value, geom="tile") +  scale_fill_gradient2(limits=c(-1, 1))

#########################################
# Model Building                        #
#########################################

# For training and testing purpose,
# split the data to 80-20

library(caret)
set.seed(1234)
intrain<-createDataPartition(y=cust_data$Churn,p=0.8,list=FALSE, times = 1)
training<-cust_data[intrain,]
testing<-cust_data[-intrain,]

#########################################
# Model 1: Logistic Regression Model    #
#########################################

# Select the features to be used based on forward selection procedure
# Akaike information criterion (AIC = 2k - 2 log L) as the choice of
# metric. Lower AIC indicates better model

fullMod = glm(Churn ~ ., data= training, family= binomial)
summary(fullMod)
intMod <- glm(Churn ~ 1, data= training, family= binomial)
summary(intMod)
fwdSelection = step(intMod, scope=list(lower=formula(intMod),upper=formula(fullMod)), direction="forward")
formula(fwdSelection)
summary(fwdSelection)

# Logistic Regression Model with selected variables

logic_reg <- glm(Churn ~ Contract + InternetService + tenure + PaperlessBilling+TotalCharges + MultipleLines + PaymentMethod +SeniorCitizen + StreamingTV + OnlineSecurity + TechSupport + StreamingMovies + MonthlyCharges + PhoneService + Dependents, data=training, family=binomial)

summary(logic_reg)

#--------------------------------------------
# Predictive Performance Measurement
#--------------------------------------------

# Run the anova() function on the model to analyze the table of deviance
anova(logic_reg, test="Chisq")

# Diagnostic Plot 
influenceIndexPlot(logic_reg, vars=c("cook","hat"), id.n = 3 )

# Influence Plot 
influencePlot(logic_reg,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

#Confidence Interval (CI) and Coefficient in scaled format
exp(confint(logic_reg))

exp(logic_reg$coefficients)

# Odd Ratio and 95% Confidence Interval
exp(cbind(OddRatio=coef(logic_reg), confint(logic_reg)))

# A logistic regression model has been built and the
# coefficients have been examined. However, some critical
# questions remain. Is the model any good? How well does
# the model fit the data? Which predictors are most
# important? Are the predictions accurate? Listed below
# are some of the Diagnostics tests.

# Churn Prediction using glm
predict <- predict(logic_reg, testing[,-20], type = 'response')

# Confusion matrix
table(testing$Churn, predict > 0.5)

# Receiver Operating Characteristic (ROC) curves

ROCRpred <- prediction(predict, testing$Churn)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
abline(a=0, b= 1)

# For accuracy, calculate the AUC (area under the curve) which
# are typical performance measurements for a binary classifier.
acc.perf <- performance(ROCRpred, measure = 'auc')
acc.perf <- acc.perf@y.values[[1]]
acc.perf

################################################
# Model 2: Support Vector Machine (SVM) Model  #
################################################

svm <- tune.svm(Churn ~., data = training, gamma = 10^2, cost=10^2, tunecontrol=tune.control(cross=10))

print(svm)
summary (svm)
svm$performances

svmfit <- svm$best.model

table(training[training$Churn,], predict(svmfit))



######################################
# Model 3: Random Forest Model       #
######################################

# Random Forest is an ensemble learning  based classification
# and regression technique. It is one of the commonly used 
# predictive modelling and machine learning technique. 

library(randomForest)
rf <- randomForest(Churn ~., data=training, ntree=500,importance=T)

print(rf)
importance(rf)

plot.new()
varImpPlot(rf, type=1, pch=19, col=1, cex=1.0, main="Variable Importance Plot")
abline(v=35, col="blue")

plot.new()
varImpPlot(rf, type=2, pch=19, col=1, cex=1.0, main="Variable Importance Plot")

################################################
# Model 4: C50 algorithm and decision rule     #
################################################

# The syntax is : C5.0 (dataframe of predictors, vector of predicted
# classes). So here we selected as input dataframe all columns except the
# one containing the churn (the 20th column), and as class we use the
# churn info 

library(c50)
c50model <- C5.0(training[,-20], training$Churn)
c50model
summary(c50model)

# Testing the model
c50pred <- predict(c50model, testing[,-20])

# Confusion Matrix
library(gmodels)
CrossTable(testing$Churn, c50pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))


#--------------------------------------------
# Models Performance Comparison
#--------------------------------------------


