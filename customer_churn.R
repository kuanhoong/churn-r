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
library(rpart)
library(C50)
library(ROCR)
library(e1071)

#load the data
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

#################################
# Data Exploratory              #
#################################

# overview of customer data
View(cust_data)
summary(cust_data)
str(cust_data)

# Correlation Matrix
corrmatrix <- round(cor(cust_data[]), digits = 2)
corrmatrix

# heatmap of correlation matrix using ggplot2
qplot(x=Var1, y=Var2, data=melt(cor(cust_data, use="p")), fill=value, geom="tile") +  scale_fill_gradient2(limits=c(-1, 1))

#########################################
# Model Building                        #
#########################################

# For training and testing purpose,
# split the data to 80-20

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

#----------------------------------------------------------------
# Predictive Performance Measurement
#----------------------------------------------------------------

# Diagnostic Plot 
influenceIndexPlot(logic_reg, vars=c("cook","hat"), id.n = 3 )

# Influence Plot 
influencePlot(logic_reg,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

#Odd Ratio

###############################################
#Model 2: Support Vector Machine (SVM) Model  #
###############################################

######################################
#Model 3: Random Forest Model        #
######################################

#Variable Importance Plot


#######################################
#C50 algorithm and decision rule      #
#######################################











