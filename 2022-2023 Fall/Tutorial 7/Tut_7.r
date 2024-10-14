######################  Tutorial 7: Tree-based Methods #########################
#################           Author: Long Ma               ######################
########### Reference：An Introduction to Statistical Learning #################
################################################################################

####################### 1 Classification Tree  #################################
################################################################################
####  Get the data we want to use
####  we want to predict the sales of cars by other features
library(ISLR)
Data001 <- Carseats
High <- as.factor(ifelse(Data001$Sales <= 8, "No", "Yes"))
Data002 <- data.frame(Data001, High)

####  Build up a single classification tree
####  install.packages("tree")
library(tree)
tree.carseats <- tree(High ~ . - Sales, Data002)
summary(tree.carseats)

####  Plot the results
plot(tree.carseats)
text(tree.carseats, pretty = 0)

####  Use this single tree on test set
set.seed(911)
train <- sample(1:nrow(Data002), 200)
Testset <- Data002[-train, ]
High.test <- High[-train]

TrainTree <- tree(High ~ . -Sales, Data002, subset = train)
plot(TrainTree)
text(TrainTree, pretty = 0)

Pred001 <- predict(TrainTree, Testset, type = "class")
table(Pred001, High.test)

#### Then, consider the pruning of the tree
set.seed(1997)
cv.Car <- cv.tree(TrainTree, FUN = prune.misclass)
cv.Car

par(mfrow = c(1, 2))
plot(cv.Car$size, cv.Car$dev, type = "b")
plot(cv.Car$k, cv.Car$dev, type = "b")

prune.Car <- prune.misclass(TrainTree, best = 6)

par(mfrow = c(1, 1))
plot(prune.Car)
text(prune.Car, pretty = 0)

Pred002 <- predict(prune.Car, Testset, type = "class")
table(Pred002, High.test)

####  Another way to build up a single tree and plot it
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

set.seed(1997)
tree.carseats2 <- rpart(High ~ . - Sales, data = Data002, subset = train, 
                        method = "class", parms = list(split = "gini"))
tree.carseats2$cptable
plotcp(tree.carseats2)

tree.prune <- prune(tree.carseats2, cp = 0.088)

prp(tree.prune, type = 2, extra = 104, fallen.leaves = TRUE, main = "Tree 1")

Pred003 <- predict(tree.prune, Testset, type = "class")
table(Pred003, High.test, dnn = c("Predicted", "Actual"))

########################### 2 Regression Tree  #################################
################################################################################
####  Revisit the data set "Boston"
library(MASS)
set.seed(911)
train <- sample(1:nrow(Boston), nrow(Boston)/2)

####  Build up a regression tree
library(tree)
tree.boston <- tree(medv ~ ., data = Boston, subset = train)
summary(tree.boston)

####  Plot this tree
plot(tree.boston)
text(tree.boston, pretty = 0)

####  Use cross validation to get the optimal number of terminal nodes
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = "b")

prune.boston <- prune.tree(tree.boston, best = 6)
plot(prune.boston)
text(prune.boston, pretty = 0)

####  Make predictions on test set
yhat1 <- predict(tree.boston, newdata = Boston[-train, ])
yhat2 <- predict(prune.boston, newdata = Boston[-train, ])

boston.test <- Boston[-train, "medv"]

par(mfrow = c(1, 2))
plot(yhat1, boston.test)
abline(0, 1)

plot(yhat2, boston.test)
abline(0, 1)

mean((yhat1 - boston.test)^2)
mean((yhat2 - boston.test)^2)

####  https://blog.csdn.net/qq_18055167/article/details/124315335
####  You can also use rpart() to generate a regression tree
####  Please choose the " method = "anova" "
####  Also, by using the package "fancyRpartPlot", you are able to make your 
####  plots prettier and fancier.

###################### 3 Bagging and Random Forest  ############################
################################################################################
install.packages("randomForest")
library(randomForest)
set.seed(911)
bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 13,
                           importance = TRUE) ## why mtry = 13?
####  when mtry = the number of variables, then randomforest == bagging
bag.boston

####  make predictions on test set
yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])
par(mfrow = c(1, 1))
plot(yhat.bag, boston.test)
abline(0, 1)
mean((yhat.bag - boston.test)^2)

####  Change the number of trees
bag.boston <- randomForest(medv ~., data = Boston, subset = train, 
                           mtry = 13, ntree = 25)
yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])
mean((yhat.bag - boston.test)^2)

####  randomForest
set.seed(911)
rf.boston <- randomForest(medv ~., data = Boston, subset = train,
                          mtry = 6, importance = TRUE)
yhat.rf <- predict(rf.boston, newdata = Boston[-train, ])
mean((yhat.rf - boston.test)^2)

importance(rf.boston)
varImpPlot(rf.boston)

#### ipred/adabag can also be used in bagging

############################### 4 Boosting  ####################################
################################################################################
install.packages("gbm")
library(gbm)

set.seed(911)
boost.boston <- gbm(medv ~ ., data = Boston[train, ], distribution = "gaussian",
                    n.trees = 5000, interaction.depth = 4)
summary(boost.boston)

par(mfrow = c(1, 2))
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")

yhat.boost <- predict(boost.boston, newdata = Boston[-train, ], n.trees = 5000)
mean((yhat.boost - boston.test)^2)

boost.boston <- gbm(medv ~., data = Boston[train, ], distribution = "gaussian", 
                    n.trees = 5000, interaction.depth = 4, shrinkage = 0.2,
                    verbose = F)
yhat.boost <- predict(boost.boston, newdata = Boston[-train, ], n.trees = 5000)
mean((yhat.boost - boston.test)^2)

####  highly recommand you: adaboost, xgboost ......
####  there are lots of materials on the internet, you can check by yourself
