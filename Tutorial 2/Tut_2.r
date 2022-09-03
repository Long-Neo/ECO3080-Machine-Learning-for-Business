############  Tutorial 2: Descriptive Statistics in R  #########################
#################           Author: Long Ma               ######################
################# Reference： R in action (Second edition) #####################
################################################################################

############################    1.1 Sampling   #################################
# usually, we want to train our models on a subset of data (training set)
# and test them on another subset of data (test set/ prediction set)
# Set work path
setwd("YourPathHere")
# import csv data by read.table()
credit_data <- read.table("Credit (All).csv", header = TRUE,
                 sep = ",", stringsAsFactors = FALSE)

# in order to have the same result after each experiment
set.seed(1432)

# generate a sample of this data
s0 <- sample(1:nrow(credit_data), 3, replace = FALSE)
credit_sample_test <- credit_data[s0, ]
credit_sample_train <- credit_data[-s0, ]

# other ways to do sampling 
install.packages("sampling")
library(sampling)

# Simple random sampling without replacement
s1 <- srswor(3, nrow(credit_data))
credit_sample_1_test <- getdata(credit_data, s1)
credit_sample_1_train <- getdata(credit_data, !s1)

# Simple random sampling with replacement
s2 <- srswr(3, nrow(credit_data))
credit_sample_2_test <- getdata(credit_data, s2)
credit_sample_2_train <- getdata(credit_data, !s2)

# Stratified sampling
table(credit_data$Gender) # two types
size <- c(2, 3) # the number of each type we want in our sample
s3 <- strata(credit_data, c("Gender"), size = size, method = "srswor")
credit_sample_3_test <- getdata(credit_data, s3)
index3 <- credit_sample_3_test$ID
credit_sample_3_train <- credit_data[-index3, ]

# Others: Random systematic sampling / Cluster sampling

#################    1.2 Basic statistical analysis    #########################
#####    1.2.1 For numerical variables 
# (1) easy way to get descriptive statistics
myvars <- c("Income", "Limit", "Rating")
summary(credit_data[myvars])

# (2) use "pastecs" package to get descriptive statistics
install.packages("pastecs")
library(pastecs)
stat.desc(credit_data[myvars], basic = TRUE, desc = TRUE, norm = TRUE, p = 0.05)

# (3) use "psych" package to get descriptive statistics
install.packages("psych")
library(psych)
describe(credit_data[myvars])

# Descriptive statistics by group
library(psych)
describeBy(credit_data[myvars], list(gender = credit_data$Gender))

#####    1.2.2 For categorical variables (Frequency and contingency tables)
# (1) one dimension
table1 <- table(credit_data$Ethnicity)
prop.table(table1)

# (2) two dimensions
table2 <- table(credit_data$Ethnicity, credit_data$Gender)
margin.table(table2, 1)
margin.table(table2, 2)
prop.table(table2, 1)
prop.table(table2, 2)

# (3) use "gmodels" package
install.packages("gmodels")
library(gmodels)
CrossTable(credit_data$Ethnicity, credit_data$Gender)
help(CrossTable)

#####    1.2.3 Correlations
cov(credit_data[myvars]) # covariance
cor(credit_data[myvars], method = "pearson") # pearson correlation

install.packages("corrgram")
library(corrgram)
vars2 <- c("Income", "Limit", "Rating", "Cards", "Age", "Education", "Balance")
corrgram(credit_data[vars2], order = TRUE, lower.panel = panel.shade,
         upper.panel = panel.pie, text.panel = panel.txt,
         main = "Corrgram of credit data")
# check them by yourselves

#################    1.3 Draw graphs, charts and maps    #######################

################# (1) bar chart
### Example 1: frequency of ethnicity
counts_1 <- table(credit_data$Ethnicity)
counts_1
barplot(counts_1, 
        main = "Ethnicity of users",
        xlab = "Ethnicity", ylab = "Frequency",
        horiz = FALSE)

## ggplot2 version
install.packages("ggplot2")
library(ggplot2)

ggplot(data = credit_data, mapping = aes(x = Ethnicity)) +  
  geom_bar(color = "red", fill = "yellow")
#use + to separate each layer

### Example 2: frequency of ethnicity (with gender information)
counts_2 <- table(credit_data$Gender, credit_data$Ethnicity)
counts_2
opar <- par(no.readonly = TRUE)
par(lwd = 2, cex = 1.5, font.lab = 2)
barplot(counts_2, 
        main = "Ethnicity & Gender",
        xlab = "Ethnicity", ylab = "Frequency",
        col = c("yellow", "grey"), beside = FALSE)
legend("topleft", inset = 0.05, title = "Gender", c("Male", "Female"),
       pch = c(15, 15), col = c("yellow", "grey"))

## ggplot2 version
ggplot(data = credit_data, mapping = aes(x = Ethnicity, fill = Gender)) +
  geom_bar(position = "stack") + labs(title = "Ethnicity & Gender")

### Example 3: mean bar chart
attach(credit_data)
m1 <- aggregate(Income, by = list(Ethnicity), FUN = mean)
barplot(m1$x, names.arg = m1$Group.1)
title("Mean income of each ethnicity", 
      xlab = "Ethnicity", ylab = "Mean Income")
detach(credit_data)

## ggplot2 version
attach(credit_data)
m1 <- aggregate(Income, by = list(Ethnicity), FUN = mean)
names(m1)[1] <- "Ethnicity"
names(m1)[2] <- "Mean_Income"
ggplot(data = m1, mapping = aes(x = Ethnicity, y = Mean_Income)) + 
  geom_col() + labs(title = "Mean income of each ethnicity")
detach(credit_data)

################# (2) Pie chart
counts_1 <- table(credit_data$Ethnicity)
pct <- round(counts_1/sum(counts_1)*100)
lbls <- paste(names(counts_1), " ", pct, "%", sep = "") 
pie(counts_1, labels = lbls,  main = "Ethnicity of users")

## ggplot2 version
blank_theme <- theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 14, face = "bold")
  )
tsample <- 12
ggplot(data = credit_data, mapping = aes(x = "Ethnicity", fill = Ethnicity)) +
  geom_bar(stat = "count", width = 0.5, position = 'stack', size = 6) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9"))+
  blank_theme +
  geom_text(stat = "count", aes(label = scales::percent(..count../tsample)), 
            size = 4, position = position_stack(vjust = 0.5))

################# (3) Histograms
par(mfrow = c(2, 2))

hist(credit_data$Income)

hist(credit_data$Limit,
     breaks = 6,
     col = "blue",
     xlab = "credit limit",
     main = "credit limit with 6 bins")

hist(credit_data$Rating,
     freq = FALSE, #use density but not frequency
     breaks = 6,
     col = "red",
     xlab = "rating",
     main = "Histogram of credit rating, rug plot, density curve")
# A rug plot is a one-dimensional representation of the actual data values. 
# If there are many tied values(same values), jitter the data on the rug plot.
rug(jitter(credit_data$Rating))
lines(density(credit_data$Rating), col = "black")

x <- credit_data$Age
h <- hist(x,
     breaks = 10,
     col = "green",
     xlab = "user age",
     main = "Histogram with normal curve and box")
# set up a grid of x-axis, from min(x) to max(x) and have 40 grids
xfit <- seq(min(x), max(x), length = 40)
# draw the normal density curve based on xgrid(xfit) using the information of 
# the credit_data$Age's mean and standard errors
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
# some adjustments
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col = "blue", lwd = 2)
box()

## ggplot2 version
x2 <- credit_data$Age
p1 <- ggplot(data = credit_data, mapping = aes(x = Income)) + 
  geom_histogram(binwidth = 20) 
p2 <- ggplot(data = credit_data, mapping = aes(x = Limit)) + 
  geom_histogram(binwidth = 1000) 
p3 <- ggplot() + 
  geom_histogram(data = credit_data, mapping = aes(x = Rating, y = ..density..),
                 binwidth = 100) +
  geom_density(data = credit_data, mapping = aes(Rating)) 
p4 <- ggplot() + 
  geom_histogram(data = credit_data, mapping = aes(Age), binwidth = 10) + 
  stat_function(fun = function(x) 
    dnorm(x, mean = mean(x2), sd = sd(x2)) * nrow(credit_data), 
    color = "darkred", size = 1)
# the 4 plots here are not exactly the same as before  

install.packages("gridExtra")
library(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol = 2)

################# (4) Box Plots and Violin Plots
# box plots
par(mfrow = c(1, 1))
boxplot(Income ~ Gender, data = credit_data,
        notch  = FALSE,
        varwidth = TRUE,
        col = "grey",
        main = "Income under different gender",
        xlab = "gender",
        ylab = "income")

## ggplot2 version
ggplot(data = credit_data, aes(x = Gender, y = Income)) + 
  geom_boxplot()

# violin plots
install.packages("vioplot")
library(vioplot)
x1 <- credit_data$Rating[credit_data$Gender == " Male"]
x2 <- credit_data$Rating[credit_data$Gender == "Female"]
par(mfrow = c(1, 1))
vioplot(x1, x2, names = c("Male", "Female"), col = "gold")
title("Violin Plots", ylab = "rating", xlab = "gender")

## ggplot2 version
ggplot(data = credit_data, aes(x = Gender, y = Rating)) + 
  geom_violin(fill = "lightblue") +
  geom_boxplot(fill = "lightgreen", width = 0.2)

################# (5) Scatter plots and Line charts
# Scatter plots
attach(credit_data)
plot(Income, Rating,
     main = "Scatter plot of Income vs. Rating",
     xlab = "Income", ylab = "Rating", pch = 19)
abline(lm(Rating ~ Income), col = "red", lwd = 2, lty = 1)
lines(lowess(Income, Rating), col = "blue", lwd = 2, lty = 2)
detach(credit_data)

## ggplot2 version
ggplot(data = credit_data, aes(x = Income, y = Rating)) + 
  geom_point(pch = 17, color = "blue", size = 2) + 
  geom_smooth(method = lm, formula = y ~ x, color = "red", linetype = 2) +
  labs(title = "Credit Data", x = "Income", y = "Rating") 

# Line charts (use orange tree data) time series data
Orange < - Orange
Orange$Tree <- as.numeric(Orange$Tree)
ntrees <- max(Orange$Tree)
xrange <- range(Orange$age)
yrange <- range(Orange$circumference)
plot(xrange, yrange,
     type = "n", # do not generate any lines or dots, just set up the axes
     xlab = "Age (days)",
     ylab = "Circumference (mm)"
)
colors <- rainbow(ntrees) # use set of colors
linetype <- c(1:ntrees)
plotchar <- seq(18, 18 + ntrees, 1) # set up a range of different types of dots 

for (i in 1:ntrees) {
  tree <- subset(Orange, Tree == i)
  lines(tree$age, tree$circumference,
        type = "b",
        lwd = 2,
        lty = linetype[i],
        col = colors[i],
        pch = plotchar[i]
  )
}
title("Tree Growth", "example of line plot")
legend(xrange[1], yrange[2], 1:ntrees,
       cex = 0.8,
       col = colors,
       pch = plotchar,
       lty = linetype,
       title = "Tree"
)

# I will not provide ggplot2 version of this example. 
# please read Chapter 3/6/11 to learn more methods of drawing graphs
# if you want to have more beautiful plots and charts
# read Chapter 19 to learn how to use ggplot2 (which is very nice package)

################# (6) Maps and Dynamic plots
# Let's do something fancy: lucky money in China
install.packages("maps")
install.packages("mapproj")
install.packages("rgdal")

library(rgdal)
library(maps)
library(mapproj)

# import map
x <- rgdal::readOGR("D:/LongMa/Machine Learning in R/Tutorial_2/bou2_4p.shp") 
china_map <- fortify(x) # transform into dataframe
china_data <- read.csv("llmoneyE.csv")

ggplot() +
  geom_polygon(data = china_map, aes(x = long, y = lat, group = group),
              fill = "white", colour = "black") + # draw map
  geom_point(data = china_data, aes(x = long, y = lat, size = money,
             fill = money, alpha = 0.4), shape = 21, colour = "black") +
  scale_size_area(max_size = 8) + 
  scale_fill_gradient2(low = "#8E0F2E", mid = "#BFBEBE", high = "#0E4E75",
                       midpoint = median(na.omit(china_data$money))) + 
  coord_map("polyconic") +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) + 
  geom_text(data = china_data, aes(x = long, y = lat, 
                                   label = province), cex = 3)

# dynamic plots: SO2 and waste water emission from 2004 to 2017 in China
install.packages("animation")
library(animation)
# you should install a software called "ImageMagick"
# Then you can run the following code to get a dynamic plot

map <- rgdal::readOGR("D:/LongMa/Machine Learning in R/Tutorial_2/bou2_4p.shp")
loc <- read.table("longlat.txt", header = TRUE, sep = ",")
so2 <- read.csv("so2E.csv", fileEncoding = "GBK")
water <- read.csv("waterE.csv", fileEncoding = "GBK")

animation::saveGIF({
  for (year in 1:14){
    file <- paste(year + 2003, "HD.png", sep = "")
    so2N <- so2[, 16 - year]
    waterN <- water[, 16 - year]
    result <- ggplot() +
      geom_polygon(data = fortify(map), aes(x = long, y = lat, group = id),
                   fill = "white", colour = "black") +
      geom_point(data = loc, aes(x = long, y = lat, size = so2N,
                                 alpha = waterN / 500000), fill = "black",
                 shape = 21, colour = "white") + 
        scale_size("SO2 emission (ton)", limits = c(0, 200),
                 range = c(0, 20)) + 
        scale_alpha("Wastewater emission (10000ton)", limits = c(0, 1),
                  range = c(0.15, 0.75),
                  labels = c(0, 125000, 250000, 375000, 500000)) +
      geom_text(data = loc, aes(x = long + 1, y = lat + 1, label = prov),
                cex = 3, colour = "gray5") +
      labs(x = "Longitude", y = "Latitude", title = paste(year + 2003,
                                            "'s emission", sep = "")) +
      coord_map(projection = "azequidistant") +
      theme(
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey85")
      )
    print(result)
  }
}, "Test1.gif", interval = 0.2, anii.width = 600, ani.height = 450)
