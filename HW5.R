#San Francisco State University
#ECON 312
#HOMEWORK 5

#Name of collaborators: Minjin Park and Nishanlang Khonglah

rm(list=ls()) #rm(list of objects) removes all objects from memory
graphics.off() #Closign all previously open graphs

#Question 1

#Loading data into R
wage <- read.csv("http://online.sfsu.edu/mbar/ECON312_files/wage21.csv")

wage$SP <- wage$SM + wage$SF #Creating Schooling of Parents variable

#Q. 1 (a)
modela <- lm(S ~ SF + SM + ASVABC, data = wage)
summary(modela)

# Pairwise correlations
cor(wage[,c("SF","SM","ASVABC")])

#Q. 1(b)
# Perfect multicollinearity
modelb <- lm(S ~ SF + SM + ASVABC + SP, data = wage)
summary(modelb)

#*********************************************************************************************************

#Question 2

#Loading data into R
multi <- read.csv("http://online.sfsu.edu/mbar/ECON312_files/multi.csv")
#Variables:
#	c - consumption
#	i - income
#	w - wealth

#*********************
#Descriptive analysis*
#*********************
#Install the package, if not already installed
if (!require("stargazer")) install.packages("stargazer") 
library("stargazer")

#Q. 2(a)
#Summary statistics table
stargazer(multi, title="Multicollinearity Data Set", 
          type = "text", digits=2, out="table_multi1.htm")

stargazer(multi, title="Multicollinearity Data Set", 
          type = "text", digits=2, out="table_multi2.htm", flip=TRUE)

#*********************
#Statistical analysis*
#*********************
#3 models
model1 <- lm(c ~ i, data = multi)
model2 <- lm(c ~ w, data = multi)
model3 <- lm(c ~ i + w, data = multi)

#Summary of the above models
stargazer(model1, model2, model3, type="text", 
          dep.var.labels="Consumption",
          covariate.labels=c("Income","Wealth"),
          out="models_multi.htm")

#********************************
#  Detecting Multicollinearity  *
#********************************
# Scatter plot
plot(multi)

#Q. 2(c)
cor(multi$i,multi$w) #Checking if there is a high pariwise correlation among regressors
#or cor(multi[,c("i","w")])

#Q. 2(d)
# Auxiliary regression
model4 <- lm(w ~ i, data=multi) #Regressing wealth on income, checking R-squared
summary(model4)

#Q. 2(e)
#Visualizing the results in the previous section
plot(w ~ i, data=multi, main="Wealth vs Income", col="blue", lwd=2)
abline(coef(model4),col="red",lwd=2)
legend("topleft", legend = c("data","fitted equation"),
       col=c("blue","red"), pch=c("o","_"))

#*********************************************************************************************************

#Question 3

#Studying the importance of race and union membership in predicting women's wage using National Longitudinal Survey of employed Women (NLSW) data

rm(list=ls()) #rm(list of objects) removes all objects from memory
graphics.off() #Closign all previously open graphs

#Loading data into R
nlsw <- read.csv("http://online.sfsu.edu/mbar/ECON312_files/nlsw88.csv")
# U.S. National Longitudinal Study of Young Women (NLSW, 1988 extract). 
# Contains observation on 2246 women in their 30s and early 40s.
# Key variables:
#   wage - hourly wage, in $
#   race - factor variable with categories {"white", "black", "other"}
#   union - factor variable with categories {"nonunion", "union" }

#Install the "stargazer" package, if not already installed
if (!require("stargazer")) install.packages("stargazer") 
library("stargazer")
stargazer(nlsw, title="NLSW88 Data Set", 
          type = "text", digits=2, out="table_NLSW1.htm")
#Inspecting factor (categorical) variables
class(nlsw$race) #"factor" means qualitative variable in R
class(nlsw$union)

summary(subset(nlsw, select=c(wage, race,union))) 
#NA denotes missing values

levels(nlsw$race) #To see levels (codes) of factor variable
levels(nlsw$union) #To see levels (codes) of factor variable

#Q. 3(a)
#Table of race (table() summarizes factor variables)
table1 <- table(nlsw$race)
table1

#Q. 3(b)
round(prop.table(table1)*100,2) #Multiplication by 100 makes %

#**********************
#Statistical analysis *
#**********************
#install.packages("car") #Need to do this only once
library(car) #Allows testing linear combinations of coefficients

#Q. 3(d)
#Creating dummy variables for race
nlsw$W <- ifelse(nlsw$race=="white", 1, 0) #Dummy for white
nlsw$B <- ifelse(nlsw$race=="black", 1, 0) #Dummy for black
nlsw$O <- ifelse(nlsw$race=="other", 1, 0) #Dummy for other

#Creating dummy for union membership
nlsw$U <- ifelse(nlsw$union=="union", 1, 0) #Dummy for union membership

#Q. 3(e)
model1 <- lm(wage ~ B + O + U + tenure, data=nlsw)
#Omitted category: white, nonunion
summary(model1)

#Q. 3(k)
#Adding intraction variables
model2 <- lm(wage ~ B+O+U+tenure+B*U+O*U, data=nlsw)
summary(model2)