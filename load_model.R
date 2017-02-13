###########################################
# Churn Analytics using R                 #
# Poo Kuan Hoong                          #
# Malaysia R User Group Meetup            #  
# http://www.github.com/kuanhoong/churn-r #
###########################################

# Start the clock!
ptm <- proc.time()
setwd('C:/Users/Kuan/Documents/GitHub/churn-r/')

#load the data
library(data.table)
cust_data1 <- fread('telco1.csv')

#########################
# Data Proprocessing    #        
#########################

# Remove Unwanted Variables
cust_data1 <- cust_data1[, -1]

# Handling Missing Values: Replace NAs as 0
cust_data1[is.na(cust_data1)] <- 0


# Recode Variables: Recode using the library(car) package
cust_data1$gender <- recode(cust_data1$gender, "'Male'=1; 'Female'=0")
cust_data1$Partner <- recode(cust_data1$Partner, "'Yes'=1; 'No'=0")
cust_data1$Dependents <- recode(cust_data1$Dependents, "'Yes'=1; 'No'=0")
cust_data1$PhoneService <- recode(cust_data1$PhoneService, "'Yes'=1; 'No'=0")
cust_data1$MultipleLines <- recode(cust_data1$MultipleLines, "'Yes'=1; 'No'=0;'No phone service'=3")
cust_data1$InternetService <- recode(cust_data1$InternetService, "'No'=0; 'DSL'=1;'Fiber optic'=2")
cust_data1$OnlineSecurity <- recode(cust_data1$OnlineSecurity, "'No'=0; 'Yes'=1;'No internet service'=2")
cust_data1$OnlineBackup <- recode(cust_data1$OnlineBackup, "'No'=0; 'Yes'=1;'No internet service'=2")
cust_data1$DeviceProtection <- recode(cust_data1$DeviceProtection, "'No'=0; 'Yes'=1;'No internet service'=2")
cust_data1$TechSupport <- recode(cust_data1$TechSupport, "'No'=0; 'Yes'=1;'No internet service'=2")
cust_data1$StreamingTV <- recode(cust_data1$StreamingTV, "'No'=0; 'Yes'=1;'No internet service'=2")
cust_data1$StreamingMovies <- recode(cust_data1$StreamingMovies, "'No'=0; 'Yes'=1;'No internet service'=2")
cust_data1$Contract <- recode(cust_data1$Contract, "'Month-to-month'=0; 'One year'=1;'Two year'=2")
cust_data1$PaperlessBilling <- recode(cust_data1$PaperlessBilling, "'Yes'=1; 'No'=0")
cust_data1$PaymentMethod <- recode(cust_data1$PaymentMethod, "'Electronic check'=1; 'Mailed check'=2;'Bank transfer (automatic)'=3; 'Credit card (automatic)'=4")

#########################
# load model            #        
#########################
load('churnmodel.rda')

#logic_reg <- glm(Churn ~ Contract 
#                        + InternetService
#                        + tenure
#                        + PaperlessBilling
#                        + TotalCharges
#                        + MultipleLines
#                        + PaymentMethod
#                        + SeniorCitizen
#                        + StreamingTV
#                        + OnlineSecurity 
#                        + TechSupport
#                        + StreamingMovies
#                        + MonthlyCharges
#                        + PhoneService
#                        + Dependents

#for glm, requires to select the same variables used in training
cust_data1 <- cust_data1[,c("Contract","InternetService","tenure","PaperlessBilling","TotalCharges","MultipleLines","PaymentMethod","SeniorCitizen","StreamingTV","OnlineSecurity","TechSupport","StreamingMovies","MonthlyCharges","PhoneService","Dependents","Churn")]

#churn prediction use glm
cust_data1$Churn <- predict(logic_reg, cust_data1[, -16], type = 'response')

#set threshold: Greater than 0.5, Churn = YES (1)
cust_data1[cust_data1$Churn>0.5] <- 1
cust_data1[cust_data1$Churn<0.5] <- 0

print(cust_data1$Churn)

# Stop the clock
proc.time() - ptm
 


