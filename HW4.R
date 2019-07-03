#San Francisco State University
#ECON 312       
#FALL 2018

#Problem Set 4
#Name of collaborators:
# 1. Minjin Park
# 2. Nishanlang Khonglah


######### *********Question 10********* ##########

rm(list=ls()) #rm(list of objects) removes all objects from workspace
graphics.off() #Closes all previously opened graphs

#The command below reads the data set in .csv, and stores the data set as a data frame called “wage”
wage <- read.csv("http://online.sfsu.edu/mbar/ECON312_files/wage21.csv")

#Solution to Q.10(a)
model1 <- lm(WEIGHT85 ~ HEIGHT, data = wage) #OLS estimation
b <- coef(model1) #Storing the OLS coefficients in vector b
b  #Displaying the estimated coefficients
summary(model1)#Presenting the summary statistics of model1

#Solution to Q.10(b)
#Creating new variables for  the log of WEIGHT85 and the log of HEIGHT
wage$LWEIGHT85 <- log(wage$WEIGHT85)
wage$LHEIGHT <- log(wage$HEIGHT)
model2 <- lm(LWEIGHT85 ~ LHEIGHT, data=wage)#OLS Estimation

#Alternative to creating new variables for log of WEIGHT85 and the log of HEIGHT
#model2 <-lm(log(WEIGHT85) ~ log(HEIGHT), data=wage)

b2<- coef(model2) #Storing the OLS coefficients in vector b
b2  #Displaying the estimated coefficients
summary(model2)#Presenting the summary statistics of model2

#Solution to Q.10(f)
#*******Two sided test************
# H0: Beta2 = 0
# H1: Beta2 != 0
#**********************************

#Defining the significance level of the test
alpha <- 0.05

#Calculating degrees of freedom of the RSS
df <- df.residual(model1)

#Lower tail critical value
t_c <- qt(alpha, df, lower.tail = FALSE)

#Extracting the estimated standard error of the slope coefficient
se <- summary(model1)$coefficients["HEIGHT",2]

#Null hypothesis
beta_0 <- 0 

#Obtaining confidence intervals
confint(model1,level=0.95)

#Solution to Q.10(g) Calculating the realized value of the test statistic
t_value <- (coef(model1)[2] - beta_0)/se

######### *********Question 11********* ##########
#install.packages("stargazer")
model1 <- lm(log(EARNINGS) ~ S, data = wage)
model2 <- lm(log(EARNINGS) ~ S + EXP, data = wage)
model3 <- lm(log(EARNINGS) ~ S + EXP + SM + SF, data = wage)

library(stargazer)
stargazer(model1, model2, model3, type="text",
          title="Earnings Models in R",
          dep.var.labels="log(EARNINGS)",
          out="models_EARNINGS.htm", digits=4)

######### *********Question 12********* ##########
library(foreign)

rm(list=ls()) #rm(list of objects) removes all objects from workspace
graphics.off() #Closes all previously opened graphs

#Reading a dataset in STATA format and storing the data set as a data frame called “wage”
wage <- read.dta("http://online.sfsu.edu/mbar/ECON312_files/wage21.dta")

#Creating the dependent variable ln(EARNINGS)
wage$LEARNINGS <- log(wage$EARNINGS)

model1 <- lm(log(EARNINGS) ~ S, data = wage)
model2 <- lm(log(EARNINGS) ~ S + EXP, data = wage)
model3 <- lm(log(EARNINGS) ~ S + EXP + SM + SF, data = wage)
library(stargazer)
stargazer(model1, model2, model3, type="text",
          title="Earnings Models in STATA",
          dep.var.labels="log(EARNINGS)",
          out="models_EARNINGS.htm", digits=4)