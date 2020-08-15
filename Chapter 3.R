# Follows An Introduction to Statistical Learning
# with Applications in R
# By Gareth James, Daniela Witten, Trevor Hastie and Robert Tibshirani

# http://faculty.marshall.usc.edu/gareth-james/ISL/ISLR%20Seventh%20Printing.pdf

# good notes
# https://tdg5.github.io/stats-learning-notes/

library(MASS)
library(ISLR)
library(tidyverse)
library(QuantPsyc)


##### CONCEPTUAL---------------------------------------------------------------

#1) The p-values correspond to the null hypothesis that TV, radio, and newspaper are each
#     individually related to the dependent variable in the presense of the other two 
#     covariates

#2) In KNN regression, the dependent variable is a continuous variable while in classification
#     it is a categorical variable. KNN regression averages out the values of the N nearest 
#     neighbors while KNN classification takes the mode value.

#3a) GPA (20) IQ (.07) Gender (35) GPA:IQ (0.01) GPA:Gender (-10)
# iii is correct because of the last interaction term between GPA and 
# Gender with a negative coefficient
#b) (Salary | Female, IQ=110, GPA=4.0) = 50 + 4*20 + 110*0.07 + 1*35 + 4*110*0.01 + 4*1*(-10)
#     = 137.1
#c) False, it is dependent upon the p-value or t-statistic. Also, although the coefficient is 
#     relatively small, GPA and IQ can have relatively high variable values which are then 
#     multiplied by the coefficient


#4a) I would expect the training RSS to be lower in the cubic regression since it includes
#       more parameters and can therefore more closely align with the training data
#b) I would expect the test RSS to be lower in the linear regression since the true
#       underlying relationship is linear
#c) Once again, in the training RSS I would expect the cubic regression to fit better
#       since it contains more parameters
#d) There is not enough information to tell since it is dependent on the specifics
#       of the true underlying relationship

#5) Answered on paper

#6) In simple linear regression there is only one regressor. Therefore
#     passing through the point (x-bar, y-bar) would give beta-hat1 a value of 0. 
#     This leads to y equal to the intercept (beta-hat0)

#7) Answered on paper


