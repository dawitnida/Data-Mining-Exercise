### Data Mining and Text Mining
# Assignment 2: predictive analysis
# Dawit Nida
# Eyob Woldegiorgis
# February 2015
###


### 0. Initialization

# Set working directory to location of data files. Change this! (note: use /, not \)
# setwd("C:/Projects/DM_Assign1/Assignment2")
setwd("X:/Github/RWorkspaces/DoneAssign/DM_Assign1/Assignment2")

### 1. Fitting a function / regression

## 1.1 Read temperature data
# Read first data file as training set
temps_train = read.csv("temps1.csv")

# Read second data file as validation set
temps_validation = read.csv("temps2.csv")

ls()

## 1.2 Polynomial regression
# Definition of R function that fits a polynomial function of given degree
# Note: Run/Copy-paste the whole function definition at once
fit_polynomial = function(degree){
  xpoints = seq(0.5,12.5,0.1)
  
  # Plot data points in training set as circles
  plot(temps_train, main=paste("Degree =",degree))
  # Fit polynomial function to training data
  model = lm(temp ~ poly(month, degree), data=temps_train)
  # Plot fitted polynomial as curves
  lines(xpoints, predict(model, data.frame(month=xpoints)))
  # Optional: plot validation points in blue
  #points(temps_validation, col="blue")
  
  # Calculate Least Squares Error on train and validation set
  train_error = sum((temps_train$temp-predict(model))^2)
  validation_error = sum((temps_validation$temp-predict(model))^2)
  
  # Print results
  cat("Polynomial degree: ", degree,"\n")
  cat("Training error: ", train_error,"\n")
  cat("Validation error: ", validation_error,"\n\n")
  
  # Return results
  return(data.frame(degree=degree, train_error=train_error, validation_error=validation_error))
} # End of function definition


# Use this to plot in a new window
windows()
# Use this to place multiple plots in a grid
par(mfrow=c(3,4))

# Initialize errors table
errors = data.frame(degree=c(), train_error=c(), validation_error=c())

# Run function fitting polynomials of degrees 1 to 11
for(deg in 1:11){
  # Append results to error table
  errors = rbind(errors, fit_polynomial(deg))
}


## 1.3 Evaluation
# Show error table
View(errors)

# Plot errors on logaritmic scale
plot(errors$validation_error, type="l", col="red", log="y", ylab="Error", xlab="Degree")
lines(errors$train_error)

# Plot errors on logaritmic scale
plot(errors$train_error, type="l", col="green", log="y", ylab="Error", xlab="Degree")
lines(errors$validation_error)

### 2. Classification

## 2.1 Initialization

# Read diabetes data, make sure the data file is in the right directory
data = read.csv("diabetes.csv")

# Set type of diabetes variable to categorical
data$diabetes = as.factor(data$diabetes)


# Install and/or load packages required for classification
if(!require(MASS)){ install.packages("MASS"); library(MASS) }
if(!require(klaR)){ install.packages("klaR"); library(klaR) }
if(!require(neuralnet)){ install.packages("neuralnet"); library(neuralnet) }


## 2.2 Visual classification
# Train LDA classifier on 2 variables, 120 rows;
# visualize decision boundrary and data points in training set;
# misclassifications shown as red labels, class means as black dots.

partimat(diabetes ~ plasma.glucose + bmi, data=data[1:120,], method="lda", prec=300, plot.control=c(cex=0.7))

### Accuracy
accuracy = (60+30)/(30+60+11+19)


## 2.3 Classification on more variables

# Randomly shuffle order of data points
shuffled_data = data[sample(1:nrow(data)),]

# Standardize input variables

shuffled_data[,1:8] = scale(shuffled_data[,1:8])
# - centered to mean 0
summary(shuffled_data)

# - scaled to unit variance / standard deviation, i.e., 1
lapply(shuffled_data[,1:8], sd)

# Save data points without known diagnosis separately
unknown_data = shuffled_data[which(is.na(shuffled_data$diabetes)),]
unknown_data

# Filter out rows with missing values
shuffled_data = na.omit(shuffled_data)
View(shuffled_data)

# Choose split point for data set at 80%
split_point = round(nrow(shuffled_data)*0.8)
split_point

# Split data set into train, validation and test set
train_set = shuffled_data[1:split_point,]
test_set = shuffled_data[(split_point+1):nrow(shuffled_data),]

# Validation set (not needed for LDA)
#valid_set = shuffled_data[(split_point1+1):split_point2,]


# Train LDA classifier (diabetes variable as target)
lda_model = lda(diabetes ~ ., data=train_set)

# Predict on test set
prediction = predict(lda_model, test_set)$class


## 2.4 Evaluation
# Create contingency table, rows for prediction, columns for observation
contab = table(prediction, observation=test_set$diabetes)
View(contab)

### Accuracy
class_accuracy = (93+32)/(93+32+22+6)
class_accuracy

summary(test_set$diabetes)
pred_accuracy = 93/98
pred_accuracy


# Calculate prediction error rate
p_error = 1-sum(contab*diag(nrow(contab)))/sum(contab) 
p_error

## Comparison with guess on a priori most probable class

# Calculate prior probability of class 1 (diabetes)
prior_prob = mean(train_set$diabetes == 1)
prior_prob


# Calculate contingency table for classification based on consistent guess on class 1
# Note: table rows with all zeros are not shown!
guess = rep(1, nrow(test_set))
test_tab = table(guess, observation=test_set$diabetes)
test_tab
class_accuracy = (93+32)/sum(test_tab) 
class_accuracy

diff_pro = class_accuracy - prior_prob
diff_pro

## Preferences for different error types

threshold_1 = 0.25
class_1_posteriors = predict(lda_model, test_set)$posterior[,2]
prediction = (class_1_posteriors > threshold_1)*1
thresh_1_tab = table(prediction, observation=test_set$diabetes)


threshold_2 = 0.50
prediction_2 = (class_1_posteriors > threshold_2)*1
thresh_2_tab = table(prediction_2, observation=test_set$diabetes)

threshold_3 = 0.75
prediction_3 = (class_1_posteriors > threshold_3)*1
thresh_3_tab = table(prediction_3, observation=test_set$diabetes)

## Predicting unknown data
# Posterior probabilities for unknown data points
predict(lda_model, unknown_data)$posterior

### Accuracy
#thre_1 = sum(contab*diag(nrow(thresh_1_tab)))/sum(thresh_1_tab) 
#thre_2 = sum(contab*diag(nrow(thresh_2_tab)))/sum(thresh_2_tab) 
#thre_3 = sum(contab*diag(nrow(thresh_3_tab)))/sum(thresh_3_tab) 

thre_1 = (71+42)/(71+42+28+12)
thre_2 = (91+27)/(91+27+8+27)
thre_3 = (97+19)/(97+19+2+35)

thre_1
thre_2
thre_3


## Interpretability
# Inspect coefficients for LDA classifier, i.e. descriminative power of individual variables
conef_lda = coef(lda_model)
lda_model_diab_1 = lda(diabetes==1 ~ ., data=train_set)
conef_lda = coef(lda_model_diab_1)
summary(conef_lda)

## Neural network
# Convert diabetes variable from factor to numeric type
train_data = train_set
train_data$diabetes = as.numeric(train_data$diabetes)-1

# Train network
net = neuralnet(diabetes ~ pregnancies + plasma.glucose + blood.pressure + 
                  triceps.skin.thickness + insulin + bmi + diabetes.pedigree + age, 
                train_data, hidden=4, lifesign="full", threshold=0.002)

# Plot network topology and weights
plot(net)

# Predict validation set with network
valid_res = compute(net, valid_set[,1:8])

# Prediction output signal
valid_res$net.result

# Classify by threshold
prediction = (valid_res$net.result[,1]>0.5)*1

# Contingency table
contab = table(prediction, observation=valid_set$diabetes)

