#### Data Mining and Text Mining course 2015 - Assignment 1

### 0. Initialization

## Get Working Directory
getwd()

# Set working directory to location of data files (note: use /, not \)
setwd("C:/Users/sronnqvi/Downloads")
## Alternatively: 
#   In R,       File->Change dir
#   In RStudio, Session->Set Working Directory


# Read data
data = read.csv("diabetes.csv")


# display files 
ls()
objects()

### 1. Simple operations for practice, run the pieces of code them one by one and observe

# Inspect the names of the variables in the loaded data frame called 'data'
names(data)


class(data)

# Count the number of columns and rows in the data set
ncol(data)
nrow(data)

# Inspect top rows of the data frame
head(data)

# Inspect the variable called 'age' (20 first elements)
head(data$age, 20)

# The same can be done like this, as age is the 3rd variable. Returns a vector.
data[1:20,3]

# Select multiple columns of the data frame. Returns a data frame (a table).
data[1:20, c(3,5)]

data[1:10, c(5)]

# Vector arithmetic: Multiply all elements in the age vector with a number
data[1:20, 3] * 2

# Add elements of two vectors together
data[1:10, 3] + data[11:20, 3]

# Apply the square root function on all elements in a vector
sqrt(data[1:10, 3])

# Calculate the sum of the result
sum(sqrt(data[1:10, 3]))

# Calculate total number of pregnancies in the data set and save the result under a new name
num_pregs <- sum(data$pregnancies)

# Check that it's there
num_pregs

# Calculate the average number of pregnancies
num_pregs/nrow(data)

# Same thing but easier
mean(data$pregnancies)

# Find rows with missing values in diabetes variable
data[which(is.na(data$diabetes)),]

# Filter out missing values (NA) in diabetes variable
na.omit(data$diabetes)

# Calculate the number of people under 30
length(which(data$age < 30))

# Calculate the number of people under 30 who haven't been pregnant
length(which(data$age < 30 & data$pregnancies == 0))

# bar plot
barplot(table(data$bmi), xlab = "BMI", ylab="Frequency", col = "blue", main="Bar chart")
?barplot


hist(data$age, right=FALSE, col="green")

boxplot(data ~ data, ylab="Diabets", data =data, col=1.5)

?state
length(state.area)
plot(state.area)
state.name

length(which.min(data$age))
state.name[8]


length(data$age[24])

min(data$triceps.skin.thickness)

nchar(data$triceps.skin.thickness)
stripchart(data$insulin, xlab="Area (sq. miles)") #see method="stack" & method="jitter" for others


boxplot(sqrt(data$bmi))
hist(sqrt(data$bmi))

### 2. Univariate analysis

## 2.1 Visualization

# Plot histogram of age variable to illustrate its distribution
hist(data$pregnancies)
hist(data$plasma.glucose)
hist(data$blood.pressure)
hist(data$triceps.skin.thickness)
hist(data$insulin)
hist(data$bmi)
hist(data$diabetes.pedigree)
hist(data$age)
hist(data$diabetes)

# Plot histograms of all variables
par(mfrow=c(4,3))
for(i in 1:ncol(data)){
  hist(data[,i], breaks=100, main=names(data)[i]);
}


## 2.2 Summary statistics

# Calculate various summary statistics for variables in a data set (or single variables)
summary(data)

# This code can be used to calculate some statistic for each variable at once
lapply(data, mean)
lapply(data, sd)

# Calculate quantiles for the age variable, provides threshold values for equally frequent intervals of the variable
quantile(data$age)


### 3. Bivariate analysis

## 3.1 Visualization

# Plot the variable age against insulin in a scatter plot
plot(data$age, data$insulin)

# Plot variables pairwise in all combinations
pairs(data[1:8],cex=0.05)

# Set color according to presence of diabetes (black=no diabetes, red=diabetes)
pairs(data[1:8], col=c("black","red")[data$diabetes+1], cex=0.05)


# Density plots to visualize the different distributions of diabetes and non-diabetes cases, including their means
par(mfrow=c(4,3))
for(i in 1:(ncol(data)-1)){
  plot(density(data[which(data$diabetes==0),i]), main=names(data)[i])
  lines(density(data[which(data$diabetes==1),i]),col="darkred")
  abline(v=mean(data[which(data$diabetes==0),i]), lty=3) # Plot vertical line at mean
  abline(v=mean(data[which(data$diabetes==1),i]), lty=3, col="red")
}


## 3.2 Summary statistics

# Correlation matrix of variables in diabetes data set
cor(data)

# Mean difference
# Values of the age variable for data points with diabetes (diabetes variable equals 1)
data$age[which(data$diabetes == 1)]

# Standardize (scale and center) each column of data separately
scaled_data <- scale(data)


# Mean difference significance test
t.test(data$age~data$diabetes)


