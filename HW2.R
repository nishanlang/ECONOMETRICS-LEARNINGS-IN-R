#San Francisco State University
#ECON 312       
#FALL 2018

                              #Problem Set 2 Question 7

rm(list=ls()) #rm(list of objects) removes all objects from workspace
graphics.off() #Closes all previously opened graphs

#The command below reads the data set in .csv, and stores the data set as a data frame called “wage”
wage <- read.csv("http://online.sfsu.edu/mbar/ECON312_files/wage21.csv")

#Solving for Q.7(c)
model1 <- lm(EARNINGS ~ S, data = wage) #OLS estimation
b <- coef(model1) #Storing the OLS coefficients in vector b
b  #Displaying the estimated coefficients

#Solving for Q.7(e)
EARN_pred <- b[1] + b[2]*16 #Calculating the prediction
EARN_pred #Display the prediction

#Solving for Q.7(f)
EARN_pred <- b[1] + b[2]*20 #Calculating the prediction
EARN_pred #Display the prediction

#Solving for Q.7(g)
summary(model1)

#Solving for Q.7(h)
plot(EARNINGS ~ S, data=wage, main="Earnings vs Schooling",
     col="blue",lwd=2)
abline(coef(model1),col="red",lwd=2)
legend("topright", legend = c("data","fitted equation"),
       col=c("blue","red"), lty = c(0,1), pch = c(1,NA),lwd=2)
grid() #Optional, add grid to figure
