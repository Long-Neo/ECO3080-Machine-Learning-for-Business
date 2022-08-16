################# Tutorial 1: How to handle data in R  #########################
#################           Author: Long Ma               ######################
################# Reference： R in action (Second edition) #####################
################################################################################

#######################    1.1 Data structure in R   ###########################

###### Vector: can store numbers, strings and logit (Same type)
vector_a <- c(1, 2, 5, 6, -2)
vector_b <- c("one", "two", "three")
vector_c <- c(TRUE, FALSE, TRUE)
# Use the index to locate specific elements in the vector
print(vector_a[3])
print(vector_b[2])
print(vector_c[1:3])

###### Matrix: can store numbers, strings and boolean (Same type, 2D)
# Construct a matrix and locate specific elements in it
matrix_y <- matrix(1:12, nrow = 3, ncol = 4)
print(matrix_y) # Default: by column
print(matrix_y[2, ])
print(matrix_y[, 3])
print(matrix_y[2, 3])
print(matrix_y[2, c(2, 4)])

# Can also fill in the elements by row or by column
cells <- c(1, 2, 3, 4)
rownames <- c("R1", "R2")
colnames <- c("C1", "C2")
matrix_x1 <- matrix(cells, nrow = 2, ncol = 2, byrow = TRUE,
                           dimnames = list(rownames, colnames))
matrix_x2 <- matrix(cells, nrow = 2, ncol = 2, byrow = FALSE,
                           dimnames = list(rownames, colnames))
# See the difference
print(matrix_x1)
print(matrix_x2)

###### Array: can store numbers, strings and boolean (Same type, >2D)
# Construct an array and locate specific elements in it
dim1 <- c("X1", "X2")
dim2 <- c("Y1", "Y2", "Y3")
dim3 <- c("Z1", "Z2", "Z3", "Z4")
array_m <- array(1:24, c(2, 3, 4), dimnames = list(dim1, dim2, dim3))
print(array_m)
print(array_m[1, 2, 4])

###### DataFrame: can store different types of data in a 2D table
# Construct a dataframe
student_id <- c(202201, 202202, 202203, 202204)
age <- c(12, 13, 10, 11)
gender <- c("male", "female", "female", "male")
class <- c("Class1", "Class1", "Class2", "Class2")
vaccinate <- c(TRUE, TRUE, TRUE, FALSE)
grade <- c("g", "e", "f", "g")
# "g" means good, "e" means excellent, "f" means fair

student_data <- data.frame(student_id, age, gender, class, vaccinate, grade)
print(student_data)

# Locate the data
# the ouput is a subset of variables (three columns)
print(student_data[1:3])
# the output is two specific variables (two columns)
print(student_data[c("student_id", "vaccinate")])
# you can also use the following way to get specific variables
print(student_data$class)
student_data2 <- data.frame(student_data$student_id, student_data$vaccinate)
print(student_data2)

# When you focus on a dataframe, please use attach() and detach()
attach(student_data)
    print(class)
    student_data3 <- data.frame(student_id, vaccinate)
detach(student_data)
print(student_data3)
# with() is also useful, please learn this command by yourselves

# use student_id as an indicator of a specific row
student_data4 <- data.frame(student_id, vaccinate, row.names = student_id)
print(student_data4)

# factorize some categorical variables (ordered or not)
# e.g. gender, class, vaccinate and grade are all categorical variables
# grade is different from others (indicating a rank: excellent > good > fair)

# Construct two dataframes and compare them
student_id <- c(202201, 202202, 202203, 202204)
age <- c(12, 13, 10, 11)
gender <- c("male", "female", "female", "male")
vaccinate <- c(TRUE, TRUE, TRUE, FALSE)
grade <- c("g", "e", "f", "g")

gender_f <- factor(gender, order = FALSE)
vaccinate_f <- factor(vaccinate, order = FALSE)
grade_f <- factor(grade, order = TRUE, levels = c("f", "g", "e"))

student_data5 <- data.frame(student_id, age, gender, vaccinate, grade)
student_data6 <- data.frame(student_id, age, gender_f, vaccinate_f, grade_f)
str(student_data5)
str(student_data6)

###### List: can store different types of data in a list
# a function's outcome can be stored in a list
# Construct a list
list_title <- "My list"
matrix_z <- matrix(1:6, nrow = 3, ncol = 2, byrow = TRUE)
vector_1 <- c(12, 13, 10, 11)
vector_2 <- c("male", "female", "female")
vector_2f <- factor(vector_2, order = FALSE)
# list(name1 = object1, name2 = object2,...) #nolint
mylist <- list(title = list_title, my_contents1 = matrix_z, vector_1, vector_2f)
print(mylist)
print(mylist[[1]])
print(mylist[["my_contents1"]])

############  1.2 Import data From outside and do something we like ###########
# Set work path
setwd("D:/LongMa/Machine Learning in R/Tutorial_1")
# import csv data by read.table()
credit_data <- read.table("RawCredit.csv", header = TRUE,
                 sep = ",", stringsAsFactors = FALSE)
# if your data is in the form of xlsx or dta,
# please either change them into csv or use other related methods
# if your data is quite huge, please refer to other appropriate methods
str(credit_data)

########### (1) change the type of your variables
# check the type
print(is.numeric(credit_data$Rating))
print(is.character(credit_data$Rating))
print(is.data.frame(credit_data))

# string to factor
credit_data$Gender <- as.factor(credit_data$Gender)
credit_data$Student <- as.factor(credit_data$Student)
credit_data$Married <- factor(credit_data$Married, order = FALSE)
credit_data$Ethnicity <- factor(credit_data$Ethnicity, order = FALSE)

# string to date
myformat <- "%Y/%m/%d"
# %Y = "xxxx", %y = "xx", %m is from 01 to 12, %d is from 01 to 31
# there are other kinds of format (please check them by yourselves)
credit_data$AppDate <- as.Date(credit_data$AppDate, myformat)
str(credit_data)

########### (2) rename your variable
names(credit_data)[1] <- "UserID"
names(credit_data)[2:3] <- c("UserIncome", "UserLimit")
print(names(credit_data))

########### (3) Merge two data sets (add column)
credit_data_addcol <- read.table("CreditAddCol.csv", header = TRUE,
                 sep = ",", stringsAsFactors = FALSE)
# use merge command to combine two data sets (we need a key to link them)
credit_data_1 <- merge(credit_data, credit_data_addcol, by = "UserID")
# use cbind command to combine two data sets
# (only require two dataframes have the same number of rows)
credit_data_2 <- cbind(credit_data, credit_data_addcol)

########### (4) Merge two data sets (add row)
credit_data_addrow <- read.table("CreditAddRow.csv", header = TRUE,
                 sep = ",", stringsAsFactors = FALSE)
# require these two dataframes have the same set of variables
credit_data_final <- rbind(credit_data_1, credit_data_addrow)

########### (5) Recoding some variables
# for example, variable "Rating" indicates the credit score of users.
# if we want to reconstruct this variable by the following criterion:
# if Rating >= 500, Rating = Good
# if 200 < Rating < 500, Rating = Medium
# if Rating <= 200, Rating = Bad
credit_data_final$Rating[credit_data_final$Rating >= 500] <- "Good"
credit_data_final$Rating[credit_data_final$Rating < 500 &
                         credit_data_final$Rating > 200] <- "Medium"
credit_data_final$Rating[credit_data_final$Rating <= 200] <- "Bad"
# Logical operaters in R:
# "&" = "and" , "|" = "or", "!" = "not", ...(check by yourselves)

########### (6) Sort you data
credit_data_sort <- credit_data_final[order(credit_data_final$UserIncome), ]
credit_data_sort2 <- credit_data_final[order(-credit_data_final$UserIncome), ]

########### (7) Reshape your Dataframe by "reshape2" (some basic idea)
# install.packages("reshape2")
library(reshape2)
# from credit_data_final, extract a subset of data
myvariable <- c("UserID", "UserIncome", "UserLimit", "AppDate")
new_credit <- credit_data_final[1:5, myvariable]
add_same_data <- read.table("CreditReshape.csv", header = TRUE,
                 sep = ",", stringsAsFactors = FALSE)
new_credit_final <- rbind(new_credit, add_same_data)

# melt the data
credit_after_melt <- melt(new_credit_final, id = c("UserID", "AppDate"))

# cast (or aggregate) the data into the form we want
# return to the original dataframe
credit_after_cast_1 <- dcast(credit_after_melt, UserID + AppDate ~ variable)
# unique row (UserID), each column is the variable with date
credit_after_cast_2 <- dcast(credit_after_melt, UserID ~ variable + AppDate)
# do some aggregation like the following
# calculate the mean of income and limit of two different time
credit_after_cast_3 <- dcast(credit_after_melt, UserID ~ variable, mean)

# you can do a lot of things to your data using reshape2
# please check by yourselves

############                    1.3 Functions                      ###########
##############################################################################

# Some basic function for math and statistics
x <- 4.125
print(abs(x)) # absolute value of x
print(sqrt(x)) # square root of x
print(ceiling(x)) # the smallest int >= x
print(floor(x)) # the biggest int <= x
print(log(x)) # ln(x)
print(exp(x)) # e^x
# ...............(Check them by yourselves)

y <- c(1, 2, 3, 4, 5)
print(length(y)) # number of elements in y vector
print(mean(y)) # mean of each elements of y
print(median(y)) # median of each elements of y
print(sd(y)) # std of y
print(var(y)) # variance of y
print(sum(y)) # sum of y
print(diff(y, lag = 1)) # difference of y (useful for time series data)
print(min(y)) # minimum of y
print(max(y)) # maximum of y
# ...............(Check them by yourselves)

# skip the part of functions dealing with strings (you will learn this in NLP)
# if you need to cope with the strings in your project,
# you can refer to some textbooks/ blogs to learn how to use this kind of things

# scaling your data (standardization) examples:
z <- c(1, 2, 3, 4, 5, 6, 7, 8)
new_z <- scale(z, center = TRUE, scale = TRUE)

# for / while / if else
# (1) "for" loop
for (i in 1:10) print("Hello World")
# (2) "while" loop
i <- 10
while (i > 0) {
    print("Hello World")
    i <- i - 1
    }
# (3) "if - else"
if (is.character(credit_data_final$Rating)) {
    credit_data_final$Rating <- as.factor(credit_data_final$Rating)
}
str(credit_data_final)

# please check other commands such as "ifelse" and "switch"

# set up your own function
# can you describe what this function does?

mystats <- function(x, parametric = TRUE, print = FALSE) {
    if (parametric) {
       mycenter <- mean(x)
       myspread <- sd(x)
    } else {
       mycenter <- median(x)
       myspread <- mad(x)
    }
    if (print & parametric) {
       cat("Mean=", mycenter, "\n", "SD=", myspread, "\n")
    } else if (print & !parametric) {
       cat("Median=", mycenter, "\n", "MAD=", myspread, "\n")
    }
    result <- list(center = mycenter, spread = myspread)
    return(result)
}

data <- c(1, 2, 3, 4, 5, 6, 7)
y <- mystats(data, parametric = TRUE, print = TRUE)
print(y)