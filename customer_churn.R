###########################################
# Churn Analytics using R                 #
# Poo Kuan Hoong                          #
# Malaysia R User Group Meetup            #  
# http://www.github.com/kuanhoong/churn-r #
###########################################

# Start the clock!
ptm <- proc.time()
setwd('C:/Users/Kuan/Dropbox/My Documents/R/Talks/churn-r')

#check for installed packages
packages <- c("caret", "data.table", "corrplot", "randomForest", "C50", "rpart", "ROCR", "e1071","gmodels")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

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
cust_data <- fread('telco.csv')

#########################
# Data Proprocessing    #        
#########################

# Remove Unwanted Variables
cust_data <- cust_data[, -1]

# Handling Missing Values: Replace NAs as 0
cust_data[is.na(cust_data)] <- 0

# Recode Variables: Replace Churn status, Yes = 1, No = 1
cust_data$Churn <- replace(cust_data$Churn, cust_data$Churn == "No", 0)
cust_data$Churn <- replace(cust_data$Churn, cust_data$Churn == "Yes", 1)
cust_data$Churn <- as.numeric(cust_data$Churn)


# Recode Variables: Recode using the library(car) package
cust_data$gender <- recode(cust_data$gender, "'Male'=1; 'Female'=0")
cust_data$Partner <- recode(cust_data$Partner, "'Yes'=1; 'No'=0")
cust_data$Dependents <- recode(cust_data$Dependents, "'Yes'=1; 'No'=0")
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
cust_data$PaperlessBilling <- recode(cust_data$PaperlessBilling, "'Yes'=1; 'No'=0")
cust_data$PaymentMethod <- recode(cust_data$PaymentMethod, "'Electronic check'=1; 'Mailed check'=2;'Bank transfer (automatic)'=3; 'Credit card (automatic)'=4")

#convert column to factor
cust_data[, 'Churn'] <- lapply(cust_data[, 'Churn'], factor)

#################################
# Data Exploratory              #
#################################

# overview of customer data
View(cust_data)
summary(cust_data)
str(cust_data)

# Correlation Matrix
corrmatrix <- round(cor(cust_data[, - 'Churn']), digits = 2)
corrmatrix

# heatmap of correlation matrix using ggplot2
png('correlation_matrix.png')
qplot(x = Var1, y = Var2, data = melt(cor(cust_data[, - 'Churn'], use = "p")), fill = value, geom = "tile") + scale_fill_gradient2(limits = c(-1, 1)) + labs(title = "Correlation Matrix")
dev.off()

#########################################
# Model Building                        #
#########################################

# For training and testing purpose,
# split the data to 80-20

library(caret)
set.seed(1234)
intrain <- createDataPartition(y = cust_data$Churn, p = 0.8, list = FALSE, times = 1)
training <- cust_data[intrain,]
testing <- cust_data[ - intrain,]

#########################################
# Model 1: Logistic Regression Model    #
#########################################

# Select the features to be used based on forward selection procedure
# Akaike information criterion (AIC = 2k - 2 log L) as the choice of
# metric. Lower AIC indicates better model

fullMod = glm(Churn ~ ., data = training, family = binomial)
summary(fullMod)
intMod <- glm(Churn ~ 1, data = training, family = binomial)
summary(intMod)
fwdSelection = step(intMod, scope = list(lower = formula(intMod), upper = formula(fullMod)), direction = "forward")
formula(fwdSelection)
summary(fwdSelection)

# Logistic Regression Model with selected variables

logic_reg <- glm(Churn ~ Contract + InternetService + tenure + PaperlessBilling + TotalCharges + MultipleLines + PaymentMethod + SeniorCitizen + StreamingTV + OnlineSecurity + TechSupport + StreamingMovies + MonthlyCharges + PhoneService + Dependents, data = training, family = binomial)

summary(logic_reg)

#--------------------------------------------
# Predictive Performance Measurement
#--------------------------------------------

# Run the anova() function on the model to analyze the table of deviance
anova(logic_reg, test = "Chisq")

# A logistic regression model has been built and the
# coefficients have been examined. However, some critical
# questions remain. Is the model any good? How well does
# the model fit the data? Which predictors are most
# important? Are the predictions accurate? Listed below
# are some of the Diagnostics tests.

# Diagnostic Plot
png('diagnostic_plots.png')
influenceIndexPlot(logic_reg, vars = c("cook", "hat"), id.n = 3)
dev.off()

# Influence Plot 
png('influence_plot_cook.png')
influencePlot(logic_reg, id.method = "identify", main = "Influence Plot - Cook", sub = "Circle size is proportial to Cook's Distance")
dev.off()

#Confidence Interval (CI) and Coefficient in scaled format
exp(confint(logic_reg))

exp(logic_reg$coefficients)

# Odd Ratio and 95% Confidence Interval
exp(cbind(OddRatio = coef(logic_reg), confint(logic_reg)))


################################################
# Model 2: Support Vector Machine (SVM) Model  #
################################################

# This process may take a very long time.
# It will return the best values to be used
# for the parameters gamma and cost.

svm <- tune.svm(Churn ~ ., data = training, seq(0.5, 0.9, by = 0.1), cost = seq(100, 1000, by = 100), kernel = "radial", tunecontrol = tune.control(cross = 10))

print(svm)
summary(svm)
svm$performances

# Find the best SVM model
svmfit <- svm$best.model


######################################
# Model 3: Random Forest Model       #
######################################

# Random Forest is an ensemble learning  based classification
# and regression technique. It is one of the commonly used 
# predictive modelling and machine learning technique. 

library(randomForest)
set.seed(1234)
rf <- randomForest(Churn ~ ., data = training, ntree = 1000, importance = T)

print(rf)
importance(rf)

plot.new()
png('Mean_Decrease_Accuracy.png')
varImpPlot(rf, type = 1, pch = 19, col = 1, cex = 1.0, main = "Mean Decrease Accuracy Plot")
abline(v = 35, col = "blue")
dev.off()

plot.new()
png('Mean_Decrease_Gini.png')
varImpPlot(rf, type = 2, pch = 19, col = 1, cex = 1.0, main = "Mean Decrease Gini Plot")
dev.off()


################################################
# Model 4: C50 algorithm and decision rule     #
################################################

# The syntax is : C5.0 (dataframe of predictors, vector of predicted
# classes). So here we selected as input dataframe all columns
# except the one containing the churn (the 20th column), and as
# class we use the churn info 

library(C50)

# run C5.0 model to derive decision rules to obtain insights
c50model <- C5.0(Churn ~ ., data = training, rules = TRUE)
c50model

#display the summary for C50
summary(c50model)

C5imp(c50model, metric = 'usage')
C5imp(c50model, metric = 'splits')

#########################################
# Models Performance Evaluation         #
#########################################

# Testing the model
glmpred <- predict(logic_reg, testing[, -20], type = 'response')
svmpred <- predict(svmfit, testing[, -20], type = 'response')
rfpred <- predict(rf, testing[, -20], type = 'response')

# Confusion Matrix

CrossTable(testing$Churn, glmpred > 0.5, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))

CrossTable(testing$Churn, svmpred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))

CrossTable(testing$Churn, rfpred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))

# Receiver Operating Characteristic (ROC) curves

svmpred <- as.numeric(levels(svmpred))[svmpred]
rfpred <- as.numeric(levels(rfpred))[rfpred]

glmpred <- prediction(glmpred, testing$Churn)
glmperf <- performance(glmpred, 'tpr', 'fpr')

svmpred <- prediction(svmpred, testing$Churn)
svmperf <- performance(svmpred, 'tpr', 'fpr')

rfpred <- prediction(rfpred, testing$Churn)
rfperf <- performance(rfpred, 'tpr', 'fpr')

plot.new()
png('ROC_curve.png')
plot(glmperf, col = 'green', lwd = 2.5)
plot(svmperf, add = TRUE, col = 'orange', lwd = 2.5)
plot(rfperf, add = TRUE, col = 'blue', lwd = 2.5)
abline(a = 0, b = 1, col = 'red', lwd = 2.5, lty = 2)

title('ROC Curve')
legend("bottomright", c("Logistic", "SVM", "RF"), lty = c(1, 1, 1), lwd = c(1.4, 1.4, 1.4), col = c('green', 'orange', 'blue'))
dev.off()

# AUC (area under the curve) Calculation Matrix

glm.perf <- performance(glmpred, measure = 'auc')
glm.perf <- glm.perf@y.values[[1]]
print(glm.perf)

svm.perf <- performance(svmpred, measure = 'auc')
svm.perf <- svm.perf@y.values[[1]]
print(svm.perf)

rf.perf <- performance(rfpred, measure = 'auc')
rf.perf <- rf.perf@y.values[[1]]
print(rf.perf)

########################################
# Save the model to file               #
########################################

save(logic_reg, file = 'churnmodel.rda')

########################################
# Session Info                         #
########################################

sessionInfo()

# Stop the clock
proc.time() - ptm

########################################
# Load Saved Model                     #
########################################

load('churnmodel.rda')

########################################
# Run Model with new dataset           #
########################################

library(data.table)
newdata <- fread.csv('telco1.csv')