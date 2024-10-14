######################  Tutorial 6: Nonlinear Methods ##########################
#################           Author: Long Ma               ######################
########### Reference：An Introduction to Statistical Learning #################
################################################################################

###################### 1 Polynomial Regression #################################
################################################################################
library(ISLR)
attach(Wage)

####  regress wage on age and its higher order terms
polyfit1 <- lm(wage ~ poly(age, 4, raw = T), data = Wage)
coef(summary(polyfit1))  ## look at the coefficients

####  prepare for the plot we want to draw
agelims <- range(age)  ## find the range of variable "age"
age.grid <- seq(from = agelims[1], to = agelims[2]) ## generate a grid for "age"

####  use predict function to get the "whole sample" fitting 
####  this is not an "actual" fitting
####  we only want to use scatters to illustrate function curves
####  we need the standard error of each fitted point, "se = True"
polypreds <- predict(polyfit1, newdata = list(age = age.grid), se = T)

####  add the standard error of each fitted value
se.bands <- cbind(polypreds$fit + 1.96*polypreds$se.fit, 
                  polypreds$fit - 1.96*polypreds$se.fit)

####  regress I(wage > 250) on age and its higher order terms
####  this is actually a classification problem
####  we will use logit regression to do that
logitpolyfit <- glm(I(wage > 250) ~ poly(age, 4, raw = T), 
           data = Wage, family = binomial(link = "logit"))

####  also use predict function to do the prediction and save standard error
logitpreds <- predict(logitpolyfit, newdata = list(age = age.grid), se = T)

####  here the standard error is for log odds ratio but not for probability
####  so here we need to do a transformation
####  firstly generate the estimated probability
pfit <- exp(logitpreds$fit)/(1 + exp(logitpreds$fit))

####  then generate the confidence interval
se.bands.logit = cbind(logitpreds$fit + 1.96*logitpreds$se.fit,
                       logitpreds$fit - 1.96*logitpreds$se.fit)
se.bands2 <- exp(se.bands.logit)/(1 + exp(se.bands.logit))

####  Plot these two graphs
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))

plot(age, wage, xlim = agelims, cex = 0.5, col = "darkgrey")
title("Degree-4 Polynomial", outer = T)
lines(age.grid, polypreds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

plot(age, I(wage > 250), xlim = agelims, type = "n", ylim = c(0, 0.2))
title("Degree-4 Polynomial for Logit Model")
points(jitter(age), I((wage > 250)/5), cex = 0.5, pch = "|", col = "darkgrey")
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands2, lwd = 1, col = "blue", lty = 3)

detach(Wage)

####  how to decide on the degree of the polynomial to use?
####  methods: (1) Cross Validation (2) ANOVA or hypotheses testing
####  another intuitive way: scatter plot (relationship between 2 vars)

fit.1 <- lm(wage ~ age , data = Wage)
fit.2 <- lm(wage ~ poly(age , 2), data = Wage)
fit.3 <- lm(wage ~ poly(age , 3), data = Wage)
fit.4 <- lm(wage ~ poly(age , 4), data = Wage)
fit.5 <- lm(wage ~ poly(age , 5), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

coef(summary(fit.5)) ## check the statistic significance of each term

############################# 2 Step function ##################################
################################################################################

table(cut(Wage$age, 4))

stepfit <- lm(wage ~ cut(age, 4), data = Wage)
coef(summary(stepfit))

steppreds <- predict(stepfit, newdata = list(age = age.grid), se = T)
se.bands3 <- cbind(steppreds$fit + 1.96*steppreds$se.fit, 
                   steppreds$fit - 1.96*steppreds$se.fit)

stepfit2 <- lm(wage ~ cut(age, 3), data = Wage)
steppreds2 <- predict(stepfit2, newdata = list(age = age.grid), se = T)

stepfit3 <- lm(wage ~ cut(age, 5), data = Wage)
steppreds3 <- predict(stepfit3, newdata = list(age = age.grid), se = T)

stepfit4 <- lm(wage ~ cut(age, 6), data = Wage)
steppreds4 <- predict(stepfit4, newdata = list(age = age.grid), se = T)

par(mfrow = c(1, 1))
plot(Wage$age, Wage$wage, xlim = agelims, cex = 0.5, col = "darkgrey")
title("Step Function")
lines(age.grid, steppreds$fit, lwd = 2, col = "blue")
lines(age.grid, steppreds2$fit, lwd = 2, col = "green")
lines(age.grid, steppreds3$fit, lwd = 2, col = "red")
lines(age.grid, steppreds4$fit, lwd = 2, col = "purple")
matlines(age.grid, se.bands3, lwd = 1, col = "blue", lty = 3)

####  how to decide the number of cut-offs? CV

############################# 3 Spline #########################################
################################################################################
install.packages("splines")
library(splines)

#### We usually use cubic spline
#### The only considerations here are:
#### "how many knots?" and "where to put the knots?"
#### suppose the following example (plot the result)

splinefit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
splinepred <- predict(splinefit, newdata = list(age = age.grid), se = T)

par(mfrow = c(1, 1))
plot(Wage$age, Wage$wage, col = "gray")
title("Cubic Spline with 3 Predetermined Knots")
lines(age.grid, splinepred$fit, lwd = 2, col = "red")
lines(age.grid, splinepred$fit + 1.96*splinepred$se, lty = "dashed", col = "blue")
lines(age.grid, splinepred$fit - 1.96*splinepred$se, lty = "dashed", col = "blue")

#### here we have 6 basis functions ? why?
#### Count the number of degrees of freedom carefully!
dim(bs(Wage$age, knots = c(25, 40, 60)))

#### firstly choose the degrees of freedom
#### then let the computer decide the location of each knot
dim(bs(Wage$age , df = 6))
attr(bs(Wage$age, df = 6), "knots")

#### if you do not want cubic spline
#### then you just set the parameter "dgree = 1" or "dgree = 2"
linearfit <- lm(wage ~ bs(age, knots = c(25, 40, 60), degree = 1), data = Wage)
linearpred <- predict(linearfit, newdata = list(age = age.grid), se = T)

par(mfrow = c(1, 1))
plot(Wage$age, Wage$wage, col = "gray")
lines(age.grid, linearpred$fit, lwd = 2, col = "red")
lines(age.grid, linearpred$fit + 1.96*linearpred$se, lty = "dashed", col = "blue")
lines(age.grid, linearpred$fit - 1.96*linearpred$se, lty = "dashed", col = "blue")

#### Natural cubic spline
splinefit2 <- lm(wage ~ ns(age, df = 4), data = Wage)
#### why the degrees of freedom is 4?
splinepred2 <- predict(splinefit2, newdata = list(age = age.grid), se = T)
lines(age.grid, splinepred2$fit, col = "red", lwd = 2)
lines(age.grid, splinepred2$fit + 1.96*splinepred2$se, lty = "dashed", col = "red")
lines(age.grid, splinepred2$fit - 1.96*splinepred2$se, lty = "dashed", col = "red")

#### Smoothing spline
attach(Wage)
plot(age, wage, xlim = agelims, cex = 0.5, col = "darkgrey")
title("Smoothing Spline")
smoothfit <- smooth.spline(age, wage, df = 16) # manually define lambda
smoothfit2 <- smooth.spline(age, wage, cv = T) # use cross validation
smoothfit2$df
lines(smoothfit, col = "red", lwd = 2)
lines(smoothfit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"), col = c("red", "blue"), 
       lty = 1, lwd = 2, cex = 0.8)
detach(Wage)

############################# 4 Local Regression ###############################
################################################################################
attach(Wage)
plot(age, wage, xlim = agelims, cex = 0.5, col = "darkgrey")
title("Local Regression")
localfit <- loess(wage ~ age, span = 0.2, data = Wage)
localfit2 <- loess(wage ~ age, span = 0.5, data = Wage)

lines(age.grid, predict(localfit, data.frame(age = age.grid)), col = "red", lwd = 2)
lines(age.grid, predict(localfit2, data.frame(age = age.grid)), col = "blue", lwd = 2)
legend("topright", legend = c("Span = 0.2", "Span = 0.5"), 
       col = c("red", "blue"), lty = 1, lwd = 2, cex = 0.8)
detach(Wage)

attach(Wage)
plot(age, wage, xlim = agelims, cex = 0.5, col = "darkgrey")
title("Local Regression")
localfit <- loess(wage ~ age, span = 0.2, degree = 0, data = Wage)
localfit2 <- loess(wage ~ age, span = 0.2, degree = 1, data = Wage)
localfit3 <- loess(wage ~ age, span = 0.2, degree = 2, data = Wage)

lines(age.grid, predict(localfit, data.frame(age = age.grid)), col = "red", lwd = 2)
lines(age.grid, predict(localfit2, data.frame(age = age.grid)), col = "blue", lwd = 2)
lines(age.grid, predict(localfit3, data.frame(age = age.grid)), col = "purple", lwd = 2)
legend("topright", legend = c("degree = 0", "degree = 1", "degree = 2"), 
       col = c("red", "blue", "purple"), lty = 1, lwd = 2, cex = 0.8)
detach(Wage)

############################# 5 GAM ############################################
################################################################################
install.packages("gam")

library(gam)
gam.m3 <- gam(wage ~ s(year , 4) + s(age , 5) + education, data = Wage)
par(mfrow = c(1, 3))
plot(gam.m3, se = TRUE , col = "blue")

gam1 <- lm(wage ~ ns(year , 4) + ns(age , 5) + education, data = Wage)
par(mfrow = c(1, 3))
plot.Gam(gam1 , se = TRUE , col = "red")
