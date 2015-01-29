###
# Data Mining and Text Mining - Assignment 1
# Dawit Nida
# Eyob Woldegiorgis
# Januay 2015
###

# Get the working directory
getwd()

# Read data
data = read.csv("diabetes.csv")

# Check file exists
ls()

## Q1.a
# How many data points are in the diabetes dataset? How many
# variables, i.e., dimensions?

# Inspect the names of the variables in the loaded data frame called 'data'
vars <- names(data)
vars

# Count the number of columns and rows in the data set
num_col <- ncol(data)
num_row <- nrow(data)

## Q1.b. How many missing values are there in the diabetes variable?
miss_row <- data[which(is.na(data$diabetes)),]
miss_row

## Q1.b. How many people with diabetes have a BMI of 30 or higher in the dataset?
count_people <- length(which(data$diabetes == 1 & data$bmi >= 30 ))
count_people


data_summary <- summary(data)

## Q3.a. What is the range of age ?
age_range <- (max(data$age) - min(data$age))

blood_pressure = (data$blood.pressure)
sd(blood_pressure)

## simn = rnorm(data$blood.pressure, 69.11 , 19.35581)
## hist(simn, main="Symmetric distribution")
## ?rnorm
