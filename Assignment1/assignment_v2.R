###
# Data Mining and Text Mining - Assignment 1
# Dawit Nida
# Eyob Woldegiorgis
# February 2015
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
num_col
num_row

## Q1.b. How many missing values are there in the diabetes variable?
miss_row <- data[which(is.na(data$diabetes)),]
nrow(miss_row)

# Filter out missing values (NA) in diabetes variable
na.omit(data$diabetes)

## Q1.b. How many people with diabetes have a BMI of 30 or higher in the dataset?
count_people <- length(which(data$diabetes == 1 & data$bmi >= 30 ))
count_people

# Plot histogram of age variable to illustrate its distribution
# barplot(table(data$pregnancies), xlab = "pregnancies", ylab="Frequency", col = "blue", main="Bar chart")
# barplot(table(data$plasma.glucose), xlab = "plasma.glucose", ylab="Frequency", col = "red", main="Bar chart")
# barplot(table(data$blood.pressure), xlab = "blood.pressure", ylab="Frequency", col = "blue", main="Bar chart")
# barplot(table(data$age), xlab = "age", ylab="Frequency", col = "blue", main="Bar chart")
# barplot(table(data$insulin), xlab = "insulin", ylab="Frequency", col = "blue", main="Bar chart")
# barplot(table(data$diabetes.pedigree), xlab = "diabetes.pedigree", ylab="Frequency", col = "blue", main="Bar chart")
# barplot(table(data$bmi), xlab = "BMI", ylab="Frequency", col = "blue", main="Bar chart")
# barplot(table(data$diabetes), xlab = "diabetes", ylab="Frequency", col = "blue", main="Bar chart")

# Q2  Judging by common sense, do the distributions of values seem
# realistic, or can you identify any problems
hist(data$pregnancies)
hist(data$plasma.glucose)
hist(data$blood.pressure)
hist(data$triceps.skin.thickness)
hist(data$insulin)
hist(data$bmi)
hist(data$diabetes.pedigree)
hist(data$age)
hist(data$diabetes)

## Q3. summary statistics 
data_summary <- summary(data)
data_summary

## Q3.a. What is the range of age ?
age_range <- range(data$age)
age_range

# Filter out missing values (NA) in diabetes variable
na.omit(data$diabetes)

# relationship between age and blood pressure
plot(data$age, data$blood.pressure,
     col =c("red","blue"), 
     main="Relationship between Age & Blood pressure")

# linear model, plot
# lm(data$blood.pressure~data$age)
# y = 56.0009 + 0.3942*x

# Best fit line
# abline(lm(data$blood.pressure~data$age))

# Set color according to presence of diabetes (black=no diabetes, red=diabetes)
pairs(data[1:8], col=c("green","red")[data$diabetes+1], cex=0.05)
?pairs

# Density plots to visualize the different distributions of diabetes and non-diabetes cases, including their means
par(mfrow=c(3,3))
for(i in 1:(ncol(data)-1)){
  plot(density(data[which(data$diabetes==0),i]), main=names(data)[i])
  lines(density(data[which(data$diabetes==1),i]),col="green")
  abline(v=mean(data[which(data$diabetes==0),i]), lty=3) 
  abline(v=mean(data[which(data$diabetes==1),i]), lty=3, col="red")
}

# Correlation matrix of variables in diabetes data set
cor(data, data$plasma.glucose)
cor.test(data$age, data$pregnancies)
cor(data, data$age)

# Mean difference
# Values of the age variable for data points with diabetes (diabetes variable equals 1)

data$age[which(data$diabetes == 1)]

# Standardize (scale and center) each column of data separately
scaled_data <- scale(data)
scaled_data

# Mean difference significance test
t.test(data$pregnancies~data$diabetes)
t.test(data$plasma.glucose~data$diabetes)
t.test(data$blood.pressure~data$diabetes)
t.test(data$triceps.skin.thickness~data$diabetes)
t.test(data$insulin~data$diabetes)
t.test(data$bmi~data$diabetes)
t.test(data$diabetes.pedigree~data$diabetes)
t.test(data$age~data$diabetes)

mean_diff <- c(1.587419,30.8531,2.65238,2.46558,30.46182,4.84747,0.1230949,5.88997)
sort(mean_diff)

?plot
