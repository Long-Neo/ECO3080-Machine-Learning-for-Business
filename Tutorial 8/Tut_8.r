######################  Tutorial 8: Deep Learning  #############################
#################           Author: Long Ma               ######################
########### Reference：An Introduction to Statistical Learning #################
################################################################################

############################# 0 Set environment  ###############################
################################################################################
####  https://zhuanlan.zhihu.com/p/146326774 will be helpful
install.packages("reticulate")
install.packages("keras")
install.packages("tensorflow")
library(keras)
install_keras()

############## 1 A Single Layer Network on the Hitters Data  ###################
################################################################################
####  Get the data and split data into test set
library(ISLR2)
Gitters <- na.omit(Hitters)
n <- nrow(Gitters)
set.seed (911)
ntest <- trunc(n / 3)  ## max integer smaller than n
testid <- sample (1:n, ntest)

x <- scale(model.matrix(Salary ~ . - 1, data = Gitters))
y <- Gitters$Salary

####  Use keras to build up NN model
library(keras)
modnn <- keras_model_sequential() %>% 
  layer_dense(units = 50, # a single hidden layer with 50 hidden units
              activation = "relu", # a ReLU activation function
              input_shape = ncol(x)) %>% 
  ##  Dimensionality of the input (integer) not including the samples axis. 
  ##  "input_shape" is required when using this layer as the 1st layer.
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1)

####  We add details to modnn that control the fitting algorithm
modnn %>% compile(loss = "mse",
                  optimizer = optimizer_rmsprop(),
                  metrics = list("mean_absolute_error"))

history <- modnn %>% fit(
  x[-testid, ], y[-testid], epochs = 1500, batch_size = 32,
  validation_data = list(x[testid, ], y[testid])
)

####  Display the mean absolute error for the training and test data
library(ggplot2)
plot(history)

####  Predict from the final model  (TR = 228.0111)
npred <- predict(modnn, x[testid, ])
mean(abs(y[testid] - npred))

####  Compare with other methods
####  Linear Regression (TR = 210.5706)
lfit <- lm(Salary ~ ., data = Gitters[-testid, ])
lpred <- predict(lfit, Gitters[testid, ])
with(Gitters[testid, ], mean(abs(lpred - Salary)))

####  Lasso Regression (TR = 192.1307)
library(glmnet)
cvfit <- cv.glmnet(x[-testid, ], y[-testid], type.measure = "mae")
cpred <- predict(cvfit, x[testid, ], s = "lambda.min")
mean(abs(y[testid] - cpred))

############## 2 A Multi-layer Network on the MNIST Digit Data  ################
################################################################################
####  Import the data  (3-D array)
mnist <- dataset_mnist ()
x_train <- mnist$train$x
g_train <- mnist$train$y
x_test <- mnist$test$x
g_test <- mnist$test$y
dim(x_train)
dim(x_test)

####  Reshape them into a matrix
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))
y_train <- to_categorical(g_train, 10)
y_test <- to_categorical(g_test, 10)

####  Neural networks are somewhat sensitive to the scale of the inputs.
####  Re-scale to the unit interval (0~255)
x_train <- x_train / 255
x_test <- x_test / 255

####  Set up the model with 3 layers
modelnn <- keras_model_sequential ()
modelnn %>%
   layer_dense(units = 256, activation = "relu",
                input_shape = c(784)) %>%
   layer_dropout(rate = 0.4) %>%
   layer_dense(units = 128, activation = "relu") %>%
   layer_dropout(rate = 0.3) %>%
   layer_dense(units = 10, activation = "softmax")

summary(modelnn)

####  Define the loss function
modelnn %>% compile(loss = "categorical_crossentropy",
                    optimizer = optimizer_rmsprop (), 
                    metrics = c("accuracy"))

system.time(  ##  record the time
   history <- modelnn %>%
              fit(x_train , y_train , epochs = 30, batch_size = 128,
                  validation_split = 0.2)) ## 20% of the data are used to test
plot(history, smooth = FALSE)

####  Define a function to calculate the accuracy of our model
####  Unfortunately, "predict_classes()" was removed in Tensorflow 2.6 
accuracy <- function(pred , truth) {
  mean(drop(pred) == drop(truth))
}
##  modelnn %>% predict_classes(x_test) %>% accuracy(g_test)
##  instead, you should use the following codes
y_prob <- modelnn %>% predict(x_test)
y_predict <- apply(y_prob, 1, which.max) - 1
accuracy(y_predict, g_test)

####  Compare with other methods
modellr <- keras_model_sequential () %>%
  layer_dense(input_shape = 784, units = 10, activation = "softmax")
summary(modellr)

modellr %>% compile(loss = "categorical_crossentropy",
                    optimizer = optimizer_rmsprop (), metrics = c("accuracy"))
modellr %>% fit(x_train , y_train , epochs = 30,
                  batch_size = 128, validation_split = 0.2)
y_prob2 <- modellr %>% predict(x_test)
y_predict2 <- apply(y_prob2, 1, which.max) - 1
accuracy(y_predict2, g_test)

####################### 3 Neural Networks for time series ######################
####################### ** just for your information **  #######################
################################################################################
####  Import Data and do some pre-processing work
library(ISLR2)
xdata <- data.matrix(NYSE[, c("DJ_return", "log_volume","log_volatility")])
istrain <- NYSE[,"train"]
xdata <- scale(xdata)

####  Define a function for "lags" 
lagm <- function(x, k = 1) {
  n <- nrow(x)
  pad <- matrix(NA, k, ncol(x))
  rbind(pad, x[1:(n - k), ])
}

####  Generate the data we want to use
arframe <- data.frame(log_volume = xdata[, "log_volume"],
                      L1 = lagm(xdata, 1), L2 = lagm(xdata, 2),
                      L3 = lagm(xdata, 3), L4 = lagm(xdata, 4),
                      L5 = lagm(xdata, 5))
arframe <- arframe[-(1:5), ]
istrain <- istrain[-(1:5)]

####  AR Model and R^2
arfit <- lm(log_volume ~ ., data = arframe[istrain, ])
arpred <- predict(arfit, arframe[!istrain , ])
V0 <- var(arframe[!istrain , "log_volume"])
1 - mean ((arpred - arframe[!istrain, "log_volume"])^2) / V0

####  Refit this model with additional variables
arframed <- data.frame(day = NYSE [-(1:5), "day_of_week"], arframe)
arfitd <- lm(log_volume ~ ., data = arframed[istrain , ])
arpredd <- predict(arfitd , arframed[!istrain , ])
1 - mean ((arpredd - arframe[!istrain, "log_volume"])^2) / V0

####  RNN Model 
n <- nrow(arframe)
xrnn <- data.matrix(arframe[, -1])
xrnn <- array(xrnn , c(n, 3, 5))
xrnn <- xrnn[,, 5:1]
xrnn <- aperm(xrnn , c(1, 3, 2))
dim(xrnn)

model <- keras_model_sequential () %>%
   layer_simple_rnn(units = 12, input_shape = list(5, 3),
                    dropout = 0.1, recurrent_dropout = 0.1) %>%
   layer_dense(units = 1)

model %>% compile(optimizer = optimizer_rmsprop (), loss = "mse")
history <- model %>% fit(xrnn[istrain,, ], arframe[istrain, "log_volume"],
                         batch_size = 64, epochs = 200,
                         validation_data = list(xrnn[!istrain,, ], 
                                                arframe[!istrain, "log_volume"])
                         )
kpred <- predict(model, xrnn[!istrain,, ])
1 - mean ((kpred - arframe [!istrain , "log_volume"])^2) / V0

####################### 4 Convolutional Neural Networks  #######################
####################### ** just for your information **  #######################
################################################################################
####  Import data
cifar100 <- dataset_cifar100 ()
names(cifar100)

x_train <- cifar100$train$x
g_train <- cifar100$train$y
x_test <- cifar100$test$x
g_test <- cifar100$test$y
dim(x_train)
range(x_train[1,,, 1])

####  Standardize the data
x_train <- x_train / 255
x_test <- x_test / 255
y_train <- to_categorical(g_train, 100)
dim(y_train)

####  Take a look at this pictures
install.packages("jpeg")
library(jpeg)
par(mar = c(0, 0, 0, 0), mfrow = c(5, 5))
index <- sample(seq (50000) , 25)
for (i in index) plot(as.raster(x_train[i,,, ]))

####  Build up our model
model <- keras_model_sequential () %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3),
                padding = "same", activation = "relu",
                input_shape = c(32, 32, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3),
                padding = "same", activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 128, kernel_size = c(3, 3),
                padding = "same", activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 256, kernel_size = c(3, 3),
                padding = "same", activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten () %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dense(units = 100, activation = "softmax")
summary(model)

model %>% compile(loss = "categorical_crossentropy",
                  optimizer = optimizer_rmsprop (), metrics = c("accuracy"))
history <- model %>% fit(x_train , y_train , epochs = 30,
                           batch_size = 128, validation_split = 0.2)

y_prob3 <- model %>% predict(x_test)
y_predict3 <- apply(y_prob3, 1, which.max) - 1
accuracy(y_predict3, g_test)
