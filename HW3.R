#San Francisco State University
#ECON 312       
#FALL 2018

#Problem Set 3

#######****************************************************************************************************************#########
###############################################    QUESTION 3 ##################################################################
#######****************************************************************************************************************#########

rm(list=ls()) #rm(list of objects) removes all objects from workspace
graphics.off() #Closes all previously opened graphs

library(foreign)

#Reading a data set in STATA format and storing the data set as a data frame called “car”
auto <- read.dta("http://www.stata-press.com/data/r12/auto.dta")

#Q.3(a) Creating a new variable weight100, which is the weight of the vehicle in hundreds of pounds
auto$weight100 <- auto$weight/100  

#Q.3(a) Summary statistics of variables mpg, weight100, and foreign
summary(subset(auto, select=c("mpg","weight100","foreign")))

#Q. 3(c) Summary statistics of mpg by categories of foreign
tapply(auto$mpg, auto$foreign,  summary, na.rm=TRUE)

#Q. 3(e) Boxplot visualization of mpg by categories of foreign
boxplot(mpg~foreign,data=auto, 
        main="MPG by Type of Manufacturer",
        ylab="Miles Per Gallon",col=c("blue","red"))

#Q. 3(f) Regression of mpg vs weight100
model1 <- lm(mpg ~ weight100, data=auto)
summary(model1) #Output of Regression

#Q. 3(h)
#*******Two sided test************
# H0: Beta2 = 0
# H1: Beta2 != 0
#**********************************

#Defining the significance level of the test
alpha <- 0.05

#Calculating degrees of freedom of the RSS
df <- df.residual(model1)

#Q. 3(h) ii Lower tail critical value
t_c <- qt(alpha, df, lower.tail = FALSE)

#Extracting the estimated standard error of the slope coefficient
se <- summary(model1)$coefficients["weight100",2]

#Null hypothesis
beta_0 <- 0 

#Q. 3(h) iii Calculating the realized value of the test statistic
t_value <- (coef(model1)[2] - beta_0)/se 

# Visualizing the t-test
source("http://online.sfsu.edu/mbar/ECON312_files/TTestFun.R") #Attach the function used for plots
TTestFun(alpha,df,"twosided",1001,5) #Plotting the t-distribution, under H0

#Q. 3(i) 
#*******Two sided test************
# H0: Beta2 = 0
# H1: Beta2 != 0
#**********************************

#Defining the significance level of the test
alpha <- 0.01

#Calculating degrees of freedom of the RSS
df <- df.residual(model1)

#Q. 3(i) ii Lower tail critical value
t_c <- qt(alpha, df, lower.tail = FALSE)

#Extracting the estimated standard error of the slope coefficient
se <- summary(model1)$coefficients["weight100",2]

#Null hypothesis
beta_0 <- 0 

#Q. 3(i) iii Calculating the realized value of the test statistic
t_value <- (coef(model1)[2] - beta_0)/se 

# Visualizing the t-test
source("http://online.sfsu.edu/mbar/ECON312_files/TTestFun.R") #Attach the function used for plots
TTestFun(alpha,df,"twosided",1001,5) #Plotting the t-distribution, under H0

#Q. 3(j)
#*******Lower tail test************
# H0: Beta2 = 0
# H1: Beta2 < 0
#**********************************

#Defining the significance level of the test
alpha <- 0.05 

#Calculating degrees of freedom of the RSS
df <- df.residual(model1)

#Q. 3(j) ii Upper tail critical value
t_c <- qt(alpha, df, lower.tail = TRUE) 

#Extracting the estimated standard error of the slope coefficient
se <- summary(model1)$coefficients["weight100",2]

#Null hypothesis
beta_0 <- 0 

#Q. 3(j) iii Calculating the realized value of the test statistic
t_value <- (coef(model1)[2] - beta_0)/se

# Visualizing the t-test
TTestFun(alpha,df,"lower",1001,5) #Plotting the t-distribution, under H0

#######****************************************************************************************************************#########
###############################################    QUESTION 4 ##################################################################
#######****************************************************************************************************************#########


rm(list=ls()) #rm(list of objects) removes all objects from workspace
graphics.off() #Closes all previously opened graphs

#Reading the data set in .csv format, and stores the data set as a data frame called “wage”
wage <- read.csv("http://online.sfsu.edu/mbar/ECON312_files/wage21.csv")

#Q. 4(a) and (b) Regression of earnings on schooling and experience
model2 <- lm(EARNINGS ~ S + EXP, data = wage) #OLS estimation
summary(model2)

#Q. 4(c)
#*******Two sided test************
# H0: Beta3 = 0
# H1: Beta3 != 0
#**********************************

#Defining the significance level of the test
alpha <- 0.05 

#Calculating degrees of freedom of RSS
df <- df.residual(model2) 

#Q. 4(c) ii Upper tail critical value
t_c <- qt(alpha/2, df, lower.tail = FALSE) 

#Extracting the estimated standard error of slope coefficient
se <- summary(model2)$coefficients["EXP",2] 

#Null hypothesis
beta_0 <- 0 

#Calculating the realized value of the test statistic
t_value <- (coef(model2)["EXP"] - beta_0)/se 

# Visualizing the t-test
source("http://online.sfsu.edu/mbar/ECON312_files/TTestFun.R") #Attach the function used for plots
TTestFun(alpha,df,"twosided",1001,5) #Plotting the t-distribution, under H0

#Q. 4(d)
#*******Upper tail test************
# H0: Beta3 = 0
# H1: Beta3 > 0
#**********************************

#Defining the significance level of the test
alpha <- 0.05 

#Q. 4(d) ii Upper tail critical value
t_c <- qt(alpha, df, lower.tail = FALSE) 

#Extracting the estimated standard error of slope coefficient
se <- summary(model2)$coefficients["EXP",2] 

#Null hypothesis
beta_0 <- 0 

# Q. 4(d) iii Calculating the realized value of the test statistic
t_value <- (coef(model2)["EXP"] - beta_0)/se 

# Visualizing the t-test
TTestFun(alpha,df,"upper",1001,5) #Plotting the t-distribution, under H0

#Q. 4(e)
#********************************
#      Two-sided test:          *
# H0: beta3 = 3, H1: beta3 != 3 *
#********************************
# Theory tested: each year of schooling increases earnings by $3 an hour.
# This is one of the rare examples, in which H0 represents the theory (not H1).
# If you do not reject H0, then conclusion is "the theory can be correct. That is, 
# each year of schooling could increase earnings by $3 an hour"

#Defining the significance level of the test
alpha <- 0.05

#Calculating degrees of freedom of RSS
df <- df.residual(model2)

#Lower tail critical value
paste("-t_c = ", qt(alpha/2, df, lower.tail = TRUE)) 

#Upper tail critical value
paste(" t_c = ", qt(alpha/2, df, lower.tail = FALSE)) 

#Extracting the estimated standard error of slope coefficient
se <- summary(model2)$coefficients["S",2] 

#Null hypothesis
beta_0 <- 3

#Calculating the realized value of the test statistic
t_value <- (coef(model2)["S"] - beta_0)/se 

#Visualization of the test
TTestFun(alpha,df,"twosided",1001,5) #Plotting the t-distribution, under H0