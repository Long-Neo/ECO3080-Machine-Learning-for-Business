############  Tutorial 4: KNN, Logit/Probit, LDA/QDA  ##########################
#################           Author: Long Ma               ######################
########### Reference：An Introduction to Statistical Learning #################
################################################################################

# Use Lag1, Lag2 and Volume to predict Direction 
###################### 0 Data Introduction  ####################################
################################################################################
library(ISLR)
Stock_Data <- Smarket

library(stargazer)
stargazer(Stock_Data, font.size = "scriptsize")

library(corrgram)
vars <- c("Year", "Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume", "Today")
corrgram(Stock_Data[vars], order = FALSE, lower.panel = panel.shade,
         upper.panel = panel.pie, text.panel = panel.txt,
         main = "Correlation of variables")

library(ggplot2)
Stock_Data$YearD <- as.factor(Stock_Data$Year)
ggplot(data = Stock_Data, aes(x = YearD, y = Volume)) + 
  geom_boxplot()

attach(Stock_Data)
train <- (Year < 2005)
Stock_Data_test <- Stock_Data[!train, ]
Stock_Data_train <- Stock_Data[train, ]
Direction_2005 <- Direction[!train]
detach(Stock_Data)

############################### 1 KNN  #########################################
################################################################################
install.packages("kknn")
library(kknn)
knn <- kknn(Direction ~ Lag1 + Lag2 + Volume,
            Stock_Data_train, Stock_Data_test, k=3)
knn_pred <- fitted(knn)
table(Stock_Data_test$Direction, knn_pred, dnn = c("True", "Pred."))

############################### 2 Logit/Probit  ################################
################################################################################
logitreg <- glm(Direction ~ Lag1 + Lag2  + Volume,
                data = Stock_Data, 
                family = binomial(link = logit), 
                subset = train)
summary(logitreg)
logit_probs <- predict(logitreg, Stock_Data_test, type = "response")
logit_pred <- rep("Down", 252)
logit_pred[logit_probs > 0.5] <- "Up"
table(logit_pred, Direction_2005)

probitreg <- glm(Direction ~ Lag1 + Lag2 + Volume,
                 data = Stock_Data, 
                 family = binomial(link = probit), 
                 subset = train)
summary(probitreg)
probit_probs <- predict(probitreg, Stock_Data_test, type = "response")
probit_pred <- rep("Down", 252)
probit_pred[probit_probs > 0.5] <- "Up"
table(probit_pred, Direction_2005)

stargazer(logitreg, probitreg, type = "html", title = "Example")

################################## 3 LDA/QDA  ##################################
################################################################################
library(MASS)
lda <- lda(Direction ~ Lag1 + Lag2  + Volume,
                data = Stock_Data, subset = train)
lda
plot(lda)
lda_pred <- predict(lda, Stock_Data_test) 
names(lda_pred)
lda_class <- lda_pred$class
table(lda_class, Direction_2005)

qda <- qda(Direction ~ Lag1 + Lag2 + Volume,
           data = Stock_Data, subset = train)
qda
qda_pred <- predict(qda, Stock_Data_test)
table(qda_pred$class, Direction_2005)

################################## 4 Comparison  ###############################
################################################################################
install.packages("pROC")
library(pROC)

knn_roc <- roc(Stock_Data_test$Direction, as.numeric(knn_pred))
logit_roc <- roc(Stock_Data_test$Direction, logit_probs)
lda_roc <- roc(Stock_Data_test$Direction, as.numeric(lda_pred$class))
qda_roc <- roc(Stock_Data_test$Direction, as.numeric(qda_pred$class))

par(mfrow = c(2, 2))
plot(knn_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col = c("green", "red"), max.auc.polygon = TRUE,
     auc.polygon.col = "skyblue", print.thres = TRUE, main = 'ROC')
plot(logit_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col = c("green", "red"), max.auc.polygon = TRUE,
     auc.polygon.col = "skyblue", print.thres = TRUE, main = 'ROC')
plot(lda_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col = c("green", "red"), max.auc.polygon = TRUE,
     auc.polygon.col = "skyblue", print.thres = TRUE, main = 'ROC')
plot(qda_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col = c("green", "red"), max.auc.polygon = TRUE,
     auc.polygon.col = "skyblue", print.thres = TRUE, main = 'ROC')


######################### **5 Multi-class Logit  ###############################
################################################################################
# Generate a special data set
n <-1000 # define sample size
set.seed(2022) # so can reproduce the results
age <- rnorm(n, 60, 10)
blood.pressure <- rnorm(n, 125, 15)
sex <- factor(sample(c('female', 'male'), n, TRUE))
outcome <- factor(sample(c(1,2,3), n, TRUE), levels = c("1", "2", "3"),
                 labels = c("poor", "fair", "good"))
data001 <- data.frame(age, blood.pressure, sex, outcome)

######  Logit with multi-class y (without order)
install.packages("nnet")
library(nnet)
multi_logit <- multinom(outcome ~ sex + age + blood.pressure, 
                    data = data001)
summary(multi_logit)

######  Logit with multi-class y (with order)
library(MASS)

# set up regression
orderlogit <- polr(ordered(outcome) ~ sex + age + blood.pressure, 
                   data = data001)
summary(orderlogit)

# test same slope
install.packages("brant")
library(brant)
brant(orderlogit) 

# compare to the null model
nullmodel <- polr(ordered(outcome) ~ 1, data = data001)
anova(nullmodel, orderlogit)

######################### **6 Naive Bayesian Classification  ###################
################################################################################
install.packages("klaR")
library(klaR)
Bayes1 <- NaiveBayes(Direction ~ Lag1 + Lag2 + Volume, 
                         data = Stock_Data_train)
Bayes1[1:length(Bayes1)]
par(mfrow = c(3, 1))
plot(Bayes1)

pre_Bayes1 <- predict(Bayes1, Stock_Data_test)
table(Stock_Data_test$Direction, pre_Bayes1$class)
