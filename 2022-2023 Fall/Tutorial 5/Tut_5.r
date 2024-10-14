########  Tutorial 5: Resampling and  Model selection ##########################
#################           Author: Long Ma               ######################
########### Reference：An Introduction to Statistical Learning #################
################################################################################

###################### 0 Resampling Methods  ###################################
################################################################################

# LOOCV
library(ISLR)
install.packages("boot")
library(boot)

glm.fit <- glm(mpg ~ horsepower, data = Auto) # define the model
cv.err <- cv.glm(Auto, glm.fit) # do LOOCV
cv.err$delta # MSE on test set

cv.error <- rep(0, 5) # you can run the above code for different polys
for (i in 1:5) {
  glm.fit <- glm(mpg ~ poly(horsepower, i, raw = TRUE), data = Auto)
  cv.error [i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error # see the change when the model is growing nonlinear

# K-fold CV
library(boot)
set.seed(911)

cv.error.10 <- rep(0, 10) # do the same thing here
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i, raw = TRUE), data = Auto)
  cv.error.10 [i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error.10

# Bootstrap
# define a function for bootstrap
boot.fn <- function(data, index){
  return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
}
library(boot)
set.seed(911)
boot(Auto, boot.fn, 1000) # got the result from bootstrap

# compare to the non-bootstrap version
summary(lm(mpg ~ horsepower, data = Auto))$coef

#################### 1 Best Subset/Forward/Backward  ###########################
################################################################################
library(ISLR)
library(ggplot2)
Hitters <- na.omit(Hitters) # drop those observations with n.a.

install.packages("leaps")
library(leaps)
regfit.full <- regsubsets(Salary ~ ., Hitters, nvmax = 5)
summary(regfit.full)
regfit.fwd <- regsubsets(Salary ~ ., Hitters, nvmax = 5, method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary ~ ., Hitters, nvmax = 5, method = "backward")
summary(regfit.bwd)

# Do the above things by k-fold CV
# define a function of prediction under regsubsets
predict.regsubsets <- function(object, newdata, id) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

k = 10
set.seed(911)
folds <- sample(1:k, nrow(Hitters), replace = TRUE)
cv.error <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

for (j in 1:k) {
  best.fit <- regsubsets(Salary ~ ., data = Hitters[folds != j, ], nvmax = 19)
  for (i in 1:19){
    pred <- predict.regsubsets(best.fit, Hitters[folds == j, ], id = i)
    cv.error[j, i] <- mean((Hitters$Salary[folds == j] - pred)^2)
  }
}

mvalue <- apply(cv.error, 2, mean)
plot(mvalue, type = "b", col = "blue", lwd = 2,
     xlab = "Number of Features",
     ylab = "Mean Squared Error")

reg.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(reg.best, 9)

######################## 2 Ridge/Lasso/Elastic Net  ############################
################################################################################
X <- model.matrix(Salary ~ ., Hitters)[, -1]
Y <- Hitters$Salary

install.packages("glmnet")
library(glmnet)

# ridge regression
ridge.mod <- glmnet(X, Y, alpha = 0, lambda = 5)
summary(ridge.mod)
ridge.mod$beta
ridge.mod$a0

# lasso regression
lasso.mod <- glmnet(X, Y, alpha = 1, lambda = 5)
summary(lasso.mod)
plot(lasso.mod)
lasso.mod$beta
lasso.mod$a0

# elastic net regression
elasticnet.mod <- glmnet(X, Y, alpha = 0.5, lambda = 5)
summary(elasticnet.mod)
elasticnet.mod$beta
elasticnet.mod$a0

# If you want to see the shrinkage of coefficients:
grid <- 10^seq(10, -2, length = 100)
ridge.mod2<- glmnet(X, Y, alpha = 0, lambda = grid)
lasso.mod2 <- glmnet(X, Y, alpha = 1, lambda = grid)
elasticnet.mod2 <- glmnet(X, Y, alpha = 0.5, lambda = grid)

plot(ridge.mod2)
plot(lasso.mod2, lwd=2)
plot(elasticnet.mod2)

# how to find the best lambda? and alpha? use k-fold CV
set.seed(1997)
train <- sample(1:nrow(X), nrow(X)/2)
test <- (-train)
Y.test <- Y[test]

cv.out <- cv.glmnet(X[train,], Y[train], alpha = 0)
plot(cv.out)
bestlambda <- cv.out$lambda.min
bestlambda 

ridge.pred <- predict(ridge.mod, s = bestlambda, newx = X[test,])
mean((ridge.pred - Y.test)^2)

out <- glmnet(X, Y, alpha = 0)
predict(out, type = "coefficients", s = bestlambda)[1:20,]

# what about lasso?
cv.out <- cv.glmnet(X[train,], Y[train], alpha = 1)
plot(cv.out)
bestlambda <- cv.out$lambda.min
bestlambda 

lasso.pred <- predict(lasso.mod, s = bestlambda, newx = X[test,])
mean((lasso.pred - Y.test)^2)

out <- glmnet(X, Y, alpha = 1)
predict(out, type = "coefficients", s = bestlambda)[1:20,]

# leave a question here, how about elastic net?
# how to choose the best alpha?

############################### 3 PCR/PLS  #####################################
################################################################################
install.packages("pls")
library(pls)

set.seed(123)
pcr.fit <- pcr(Salary ~ ., data = Hitters, subset = train,
               scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")

pcr.pred <- predict(pcr.fit, X[test, ], ncomp = 7)
mean((pcr.pred - Y.test)^2)

set.seed(123)
pls.fit <- plsr(Salary ~ ., data = Hitters, subset = train,
               scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")

pls.pred <- predict(pls.fit, X[test, ], ncomp = 2)
mean((pls.pred - Y.test)^2)
