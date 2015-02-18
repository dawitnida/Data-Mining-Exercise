###
# Data Mining and Text Mining - Assignment 2
# Predictive Analysis
# Dawit Nida
# Eyob Woldegiorgis
# February 2015
###

#Q1. 11 degree polinomial best describes the Turku climate, because 
#     all the data points are on the line and 
#     the least square error is minimum (2.258538e-28 ) in degree 11.


#Q2. Observing from the plot, the classes seem well separated by 
#    the lineaar decision boundary. 

#Q3. FalsePositive: 11
#     TrueNegative:  ~60 
#     FalseNegative: 19
#     TruePositive: 30

#Q4.a 

#Prediction accuracy:0.1830065

#Q4.b  0.1187828137

#Q5
#
#> threshold = 0.25
#> class_1_posteriors = predict(lda_model, test_set)$posterior[,2]
#> prediction = (class_1_posteriors > threshold)*1
#> table(prediction, observation=test_set$diabetes)
#observation
#prediction  0  1
#0 71 12
#1 28 42
#            observation
#prediction_2  0  1
#0 91 27
#1  8 27
#> threshold_3 = 0.75
#> prediction_3 = (class_1_posteriors > threshold_3)*1
#> table(prediction_3, observation=test_set$diabetes)
#observation
#prediction_3  0  1
#0 97 35
#1  2 19

#               0              1
#389 0.3295869532 0.670413046798
#211 0.6216257741 0.378374225926
#370 0.9228124449 0.077187555118
#622 0.9977513939 0.002248606085
#44  0.6943849482 0.305615051786


