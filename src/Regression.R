rm(list = ls()) #clean environment

#0 Setting work directory
data.path <- "C:/Users/oboke/Desktop/R H3 26.03/MLR_data_2.txt"
setwd("C:/Users/oboke/Desktop/R H3 26.03")

#1 Read in the data file
data     <- read.table(data.path, header = TRUE)

#2 Create a plot to visually assess the dependence between the variables 

plot(data, main = "Dependency of target variable vs Individual variables")


#3 Compute the correlation between all variables using an appropriate method
p.cor <- cor(data, method = "pearson")

##### "x3 appears to be our best exploratory variable" ##### 


# 4 Carry out an exploratory regression analysis and determine the best model
## Exploratory regression analysis ## 

library(leaps)
expReg      <- regsubsets(y ~ x1 + x2 + x3 + x4 + x5, data = data)
sum.expReg  <- summary(expReg)
print(sum.expReg)

## Quality criterion to determine our best model##
sum.expReg$rsq        # Coefficient of determination (maximum)
sum.expReg$adjr2      # Adjusted coefficient of determination (maximum)
sum.expReg$cp         # Mallow's CP (Minimum)
sum.expReg$bic        # Schwartz's information criterion (Minimum)

###Based on the results, we assume that model no 4 is the best model for our case.
sum.expReg$which
best.model <- sum.expReg$which[4,]
print(best.model)


#5. Fit a linear regression model for the target variable y using the selected variables x. 
## Model Fitting ##

model.1 <- lm(y ~ x1 + x2 + x3 + x5, data = data)
summary(model.1)

#The Adjusted R-squared of model 1 is 0.9181 and almost perfect but the 
#significance level of the individual variable in x1 is low so, we fit another model
#leaving out X1

model.2 <- lm(y ~ x2 + x3 + x5, data = data)
summary(model.2)

#Model 2 has high significance level and intercept so we stop the model fitting here.


#6. What is the model equation for your final model

plot(model.2$fitted.values, data$y,
     main = "Observed vs Fitted",
     xlab = "Estimated y",
     ylab = "Fitted y")
abline(0,1)


#7. Check the normality, homoscedasticity and independence of the residuals
# Residuals Check

library(car)
# check for normality using Q-Q Plot or Shapiro.test
qqPlot(model.2,
       main = "Normality Check using Q-Q Plot")
shapiro.test(model.2$residuals)


#Test for homoscedasticity
ncvTest(model.2)


# Residual Varaince test
spreadLevelPlot(model.2)


outlierTest(model.2)           #Test for significance of most extreme observations
durbinWatsonTest(model.2)      #Test for residual auto correlation
crPlots(model.2)               # Plot for assessment of non-linearity
