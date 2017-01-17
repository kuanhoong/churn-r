###########################################
# Churn Analytics using R                 #
# Poo Kuan Hoong                          #
# Malaysia R User Group Meetup            #  
# http://www.github.com/kuanhoong/churn-r #
###########################################

setwd('C:/Users/Kuan/Documents/github/churn-r')

library(caret)
library(ggplot2)

#load the data
cust_data<-read.csv('telco.csv')
View(cust_data)
str(cust_data)

#data proprocessing

#data exploratory

#Model Building
#split data to 80-20

#########################################
#Model 1: Logistic Regression Model     #
#########################################

#Predictive Performance Measurement

#Odd Ratio

###############################################
#Model 2: Support Vector Machine (SVM) Model  #
###############################################

######################################
#Model 3: Random Forest Model        #
######################################

#Variable Importance Plot














