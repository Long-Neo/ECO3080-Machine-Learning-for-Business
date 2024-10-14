##################  Tutorial 3: Linear Regression  #############################
#################           Author: Long Ma               ######################
########### Reference：An Introduction to Statistical Learning #################
################################################################################

install.packages("ISLR")
library(MASS)
library(ISLR)
library(stargazer)

######## 3.1 Linear Regression

fix(Boston) # if you do not use Rstudio, you need to use "fix" to see a data set
names(Boston) # check the variable names

# generate a table of summary statistics in the Latex format
stargazer(Boston, font.size = "scriptsize")

# generate a correlation table
cor(Boston)

# Import the data and split is into training and test set
data1 <- Boston
nlines <- round(0.3 * nrow(data1))
set.seed(911)
sampling1 <- sample(1:nrow(Boston), nlines, replace = FALSE)
Boston_test <- data1[sampling1, ]
Boston_train <- data1[-sampling1, ]

# Simple linear regression
reg1 <- lm(medv ~ lstat, data = Boston_train) # main regression

summary(reg1) # summary results of reg1
names(reg1) # all information name of reg1
coef(reg1) # extract coefficients of reg1 (a vector)
confint(reg1) # get the interval estimation of parameters

# Make predictions on some numbers (5, 10, 15)
predict(reg1, data.frame(lstat = (c(5, 10, 15))), interval = "confidence")
predict(reg1, data.frame(lstat = (c(5, 10, 15))), interval = "prediction")

# Plot the relationship
attach(Boston_train)
plot(lstat, medv, col = "blue", pch = 20)
abline(reg1, lwd = 3, col = "red")
detach(Boston_train)

# diagnosis plots
par(mfrow = c(2,2))
plot(reg1)

# other ways to draw residual plot and studentized residual plot
par(mfrow = c(2,1))
plot(predict(reg1), residuals(reg1))
plot(predict(reg1), rstudent(reg1))

# include x^2 and other nonlinear part (the model is still linear in betas)
reg2 <- lm(medv ~ lstat + I(lstat^2), data = Boston_train) # main regression
summary(reg2)

# diagnosis plots
par(mfrow = c(2,2))
plot(reg2)

# other ways to test high-leverage points
par(mfrow = c(1,1))
plot(hatvalues(reg2))
which.max(hatvalues(reg2))

######## 3.2 Multiple Linear Regression
reg3 <- lm(medv ~ ., data = Boston)
summary(reg3)

# diagnosis plots
par(mfrow = c(2,2))
plot(reg3)

# calculate the VIF to check the collinearity
# you need to install this package before using the command
install.packages("car")
library(car)
vif(reg3)

# if you do not want "age", then you can run:
# reg4 <- lm(medv ~ . - age, data = Boston)

# interactions
reg4 <- lm(medv ~ lstat * age, data = Boston_train)
reg5 <- lm(medv ~ lstat : age, data = Boston_train)

# nonlinear part
reg6 <- lm(medv ~ poly(lstat, 5), data = Boston_train)
reg7 <- lm(medv ~ log(rm), data = Boston_train)

######## 3.2 Model Comparison in linear regression context

# We want compare 6 models:
# (1) original model (medv ~ lstat)
# (2) original model + lstat^2
# (3) original model + lstat^2 + lstat^3
# (4) original model + lstat^3 + lstat^3 + lstat^4
# .............

comp_reg1 <- lm(medv ~ lstat, data = Boston_train)
comp_reg2 <- lm(medv ~ lstat + I(lstat^2), data = Boston_train)
comp_reg3 <- lm(medv ~ lstat + I(lstat^2) + I(lstat^3), data = Boston_train)
comp_reg4 <- lm(medv ~ lstat + I(lstat^2) + I(lstat^3) + I(lstat^4), 
                data = Boston_train)
comp_reg5 <- lm(medv ~ lstat + I(lstat^2) + I(lstat^3) + I(lstat^4) + 
                  I(lstat^5), data = Boston_train)
comp_reg6 <- lm(medv ~ lstat + I(lstat^2) + I(lstat^3) + I(lstat^4) + 
                  I(lstat^5) + I(lstat^6), data = Boston_train)
stargazer(comp_reg1, comp_reg2, comp_reg3, comp_reg4, comp_reg5, comp_reg6, 
          type = "html", title = "Example")

# Can you write a function and for loop to do the above regressions?

# Make predictions on validation set
predicted_medv_1 <- data.frame(predict(comp_reg1, 
                                       data.frame(lstat = (Boston_test$lstat)), 
                                       interval = "confidence"))
Boston_test1 <- data.frame(Boston_test, predicted_medv_1$fit)
predicted_medv_2 <- data.frame(predict(comp_reg2, 
                                       data.frame(lstat = (Boston_test$lstat)), 
                                       interval = "confidence"))
Boston_test2 <- data.frame(Boston_test, predicted_medv_2$fit)
predicted_medv_3 <- data.frame(predict(comp_reg3, 
                                       data.frame(lstat = (Boston_test$lstat)), 
                                       interval = "confidence"))
Boston_test3 <- data.frame(Boston_test, predicted_medv_3$fit)
predicted_medv_4 <- data.frame(predict(comp_reg4, 
                                       data.frame(lstat = (Boston_test$lstat)), 
                                       interval = "confidence"))
Boston_test4 <- data.frame(Boston_test, predicted_medv_4$fit)
predicted_medv_5 <- data.frame(predict(comp_reg5, 
                                       data.frame(lstat = (Boston_test$lstat)), 
                                       interval = "confidence"))
Boston_test5 <- data.frame(Boston_test, predicted_medv_5$fit)
predicted_medv_6 <- data.frame(predict(comp_reg6, 
                                       data.frame(lstat = (Boston_test$lstat)), 
                                       interval = "confidence"))
Boston_test6 <- data.frame(Boston_test, predicted_medv_6$fit)

# calculate RSS on validation set
RSS1 <- sum((Boston_test1$predicted_medv_1.fit - Boston_test1$medv)^2) 
RSS2 <- sum((Boston_test2$predicted_medv_2.fit - Boston_test2$medv)^2) 
RSS3 <- sum((Boston_test3$predicted_medv_3.fit - Boston_test3$medv)^2) 
RSS4 <- sum((Boston_test4$predicted_medv_4.fit - Boston_test4$medv)^2) 
RSS5 <- sum((Boston_test5$predicted_medv_5.fit - Boston_test5$medv)^2) 
RSS6 <- sum((Boston_test6$predicted_medv_6.fit - Boston_test6$medv)^2) 
