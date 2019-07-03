#San Francisco State University
#ECON 312
#HOMEWORK 6

#Name of collaborators: Minjin Park and Nishanlang Khonglah

rm(list=ls()) #rm(list of objects) removes all objects from memory
graphics.off() #Closign all previously open graphs

#Question 6(a)

#Loading data into R
wage <- read.csv("http://online.sfsu.edu/mbar/ECON312_files/wage21.csv")

#Creating the dependent variable ln(EARNINGS)
wage$LEARNINGS <- log(wage$EARNINGS)

#Estimating the models
model1 <- lm(log(EARNINGS) ~ S, data = wage)
model2 <- lm(log(EARNINGS) ~ S + EXP, data = wage)
summary(model2)#Summarizing the estimation

#Creating publincation-quality table using the stargazer package
library(stargazer)
stargazer(model1, model2, type="text",
          title="Earnings model regressed on Schooling and Earnings model regressed on Schooling and Experience",
          dep.var.labels="log(EARNINGS)",
          out="models_HW6_Q6.htm", digits=4)

rm(list=ls()) #rm(list of objects) removes all objects from memory
graphics.off() #Closign all previously open graphs

#Question 8(a)

rm(list=ls()) #rm(list of objects) removes all objects from memory
graphics.off() #Closign all previously open graphs

#Loading data into R
wage <- read.csv("http://online.sfsu.edu/mbar/ECON312_files/wage21.csv")

#Creating Dummy for Male
wage$M <- ifelse(wage$MALE=="1",1,0) 

#Estimating the model
model3<- lm(log(EARNINGS) ~ S + EXP + M, data = wage)
summary(model3) #Summarizing the estimation

#Creating publincation-quality table using the stargazer package
library(stargazer)
stargazer(model3, type="text",
          title="Earnings model including dummy (MALE) variable",
          dep.var.labels="log(EARNINGS)",
          out="HW_6_Q8.htm", digits=4)

#Question 8(b)
#Performing model diagnostics by graphing the residuals against the fitted values
plot(model3, which = 1)

#Question 8(d)
#****************************************************************
#              White test                                       *
#****************************************************************

# Testing the heteroscedastic model
resid_hetero <- residuals(model3) #Generating residuals 
resid_hetero2 <- resid_hetero^2 #Squared residuals
Y_hetero_pred <- predict(model3) #Fitted values
Y_hetero_pred2 <- Y_hetero_pred^2 #Squared fitted values
model <- lm(resid_hetero2 ~ Y_hetero_pred + Y_hetero_pred2)
summary(model)
#Reject H0 of homo. if prob>F is smaller than alpha (sig. level)

#Creating publincation-quality table using the stargazer package
library(stargazer)
stargazer(model, type="text",
          title="White test summary",
          dep.var.labels="log(EARNINGS)",
          out="HW_6_Q8_whitetest.htm", digits=4)

#Question 8(e)
#**********************************************
#  Heteroskedasticity-robust standard errors*
#**********************************************
#Install the packages "lmtest" & "sandwich", if not already installed
if (!require("lmtest")) install.packages("lmtest") #Need to do this only once
if (!require("sandwich")) install.packages("sandwich") #Need to do this only once
if (!require("stargazer")) install.packages("stargazer") 


library(lmtest) #Package that produces standard errors
library(sandwich) #Package that produces HC Covariance Matrix
library("stargazer") #Package that makes formatted tables of regression output


# Heteroscedasticity Consistent Standard Errors
coeftest(model3,vcov(model3)) #Uncorrected SE    
coeftest(model3,vcovHC(model3,type="HC1")) #HC SE   

# Presenting the results
cov <- vcovHC(model3, type = "HC1") #Covariance matrix
robust.se <- sqrt(diag(cov)) #Extracting standard errors
stargazer(model3, model3, type="text",
          title = "Uncorrected vs Robust Standard Errors",
          intercept.bottom=FALSE,
          se=list(NULL, robust.se), 
          column.labels=c("uncorrected","robust"), align=TRUE,
          out="Robust.htm", digits=4)

#Question 8(g)
#Automated WLS
model3 <- lm(log(EARNINGS) ~ S + EXP + M, data = wage, weights=1/S^2)
summary(model3)

#Creating publincation-quality table using the stargazer package
library(stargazer)
stargazer(model3, type="text",
          title = "Weighted Least Squares Results",
          intercept.bottom=FALSE,
          column.labels   = c("OLS", "WLS"),
          dep.var.labels="Y_hetero", 
          out="models_WLS.htm", digits=4)
